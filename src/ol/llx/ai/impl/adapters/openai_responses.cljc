(ns ol.llx.ai.impl.adapters.openai-responses
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [ol.llx.ai.impl.adapters.common :as adapter-common]
   [ol.llx.ai.impl.errors :as errors]
   [ol.llx.ai.impl.schema :as schema]
   [ol.llx.ai.impl.utils.unicode :as unicode]
   [promesa.core :as p]
   [taoensso.trove :as trove]))

(defn- service-tier-multiplier
  [service-tier]
  (case service-tier
    "flex" 0.5
    "priority" 2.0
    1.0))

(defn- token-cost
  [tokens per-million multiplier]
  (* (/ (double (long (or tokens 0))) 1000000.0)
     (double (or per-million 0.0))
     multiplier))

(defn- usage->canonical
  [model usage service-tier]
  (let [cached           (long (or (get-in usage [:input_tokens_details :cached_tokens]) 0))
        input            (max 0 (- (long (or (:input_tokens usage) 0)) cached))
        output           (long (or (:output_tokens usage) 0))
        total            (long (or (:total_tokens usage) (+ input output cached)))
        rates            (:cost model)
        mult             (service-tier-multiplier service-tier)
        input-cost       (token-cost input (:input rates) mult)
        output-cost      (token-cost output (:output rates) mult)
        cache-read-cost  (token-cost cached (:cache-read rates) mult)
        cache-write-cost 0.0]
    {:input        input
     :output       output
     :cache-read   cached
     :cache-write  0
     :total-tokens total
     :cost         {:input       input-cost
                    :output      output-cost
                    :cache-read  cache-read-cost
                    :cache-write cache-write-cost
                    :total       (+ input-cost output-cost cache-read-cost cache-write-cost)}}))

(defn- map-stop-reason
  [status]
  (case status
    "completed" :stop
    "incomplete" :length
    "failed" :error
    "cancelled" :error
    "in_progress" :stop
    "queued" :stop
    (throw (ex-info "Unknown OpenAI responses stop reason" {:status status}))))

(def ^:private allowed-tool-call-id-providers
  #{:openai :openai-codex})

(defn- sanitize-and-limit-id
  [s]
  (-> (or s "")
      (str/replace #"[^A-Za-z0-9_-]" "_")
      (unicode/truncate 64)
      (str/replace #"_+$" "")))

(defn normalize-tool-call-id
  [tool-call-id target-model _source-assistant-message]
  (let [provider (keyword (or (:provider target-model) :unknown))]
    (if (and (allowed-tool-call-id-providers provider)
             (string? tool-call-id)
             (str/includes? tool-call-id "|"))
      (let [[call-id item-id] (str/split tool-call-id #"\|" 2)
            call-id           (sanitize-and-limit-id call-id)
            item-id           (sanitize-and-limit-id item-id)
            item-id           (if (str/starts-with? item-id "fc") item-id (str "fc_" item-id))
            item-id           (sanitize-and-limit-id item-id)]
        (str call-id "|" item-id))
      tool-call-id)))

(defn- normalize-cache-control
  [env cache-control]
  (cond
    (keyword? cache-control) cache-control
    (and (:env/get env)
         (= "long" ((:env/get env) "PI_CACHE_RETENTION"))) :long
    :else :short))

(defn- prompt-cache-retention
  [base-url cache-control]
  (when (and (= :long cache-control)
             (str/includes? (str/lower-case (or base-url "")) "api.openai.com"))
    "24h"))

(defn- block->input-user
  [model block]
  (case (:type block)
    :text {:type "input_text" :text (:text block)}
    :image (when (contains? (get-in model [:capabilities :input] #{}) :image)
             {:type      "input_image"
              :detail    "auto"
              :image_url (str "data:" (:mime-type block) ";base64," (:data block))})
    nil))

(defn- different-openai-model?
  [assistant-message model]
  (and (= (:provider assistant-message) (:provider model))
       (= (:api assistant-message) (:api model))
       (not= (:model assistant-message) (:id model))))

(defn- truncate-message-id
  [s idx]
  (let [fallback (str "msg_" idx)
        v        (or s fallback)]
    (if (> (count v) 64)
      (str "msg_" (Math/abs (long (hash v))))
      v)))

(defn- valid-text-signature-phase
  [phase]
  (when (#{"commentary" "final_answer"} phase)
    phase))

(defn- encode-text-signature-v1
  [env id phase]
  ((:json/encode env)
   (cond-> {:v 1 :id id}
     (valid-text-signature-phase phase) (assoc :phase phase))))

(defn- parse-text-signature
  [env signature]
  (when (string? signature)
    (if (str/starts-with? signature "{")
      (let [parsed (adapter-common/parse-json-safe env signature)
            id     (:id parsed)
            phase  (valid-text-signature-phase (:phase parsed))]
        (when (and (= 1 (:v parsed))
                   (string? id))
          (cond-> {:id id}
            phase (assoc :phase phase))))
      {:id signature})))

(defn- response-failed-message
  [chunk]
  (let [error   (get-in chunk [:response :error])
        details (get-in chunk [:response :incomplete_details])]
    (cond
      (map? error)
      (str (or (:code error) "unknown")
           ": "
           (or (:message error) "no message"))

      (some? (:reason details))
      (str "incomplete: " (:reason details))

      :else
      "Unknown error (no error details in response)")))

(defn- assistant-blocks->responses-items
  [env model assistant-message idx]
  (let [different-model? (different-openai-model? assistant-message model)]
    (->> (:content assistant-message)
         (mapcat
          (fn [block]
            (case (:type block)
              :thinking
              (if-let [sig (:signature block)]
                (let [reasoning-item (adapter-common/parse-json-safe env sig)]
                  (if (and (= "reasoning" (:type reasoning-item))
                           (:encrypted_content reasoning-item))
                    [reasoning-item]
                    []))
                [])

              :text
              (let [parsed-signature (parse-text-signature env (:signature block))]
                [(cond-> {:type    "message"
                          :role    "assistant"
                          :status  "completed"
                          :id      (truncate-message-id (:id parsed-signature) idx)
                          :content [{:type "output_text" :text (:text block) :annotations []}]}
                   (:phase parsed-signature) (assoc :phase (:phase parsed-signature)))])

              :tool-call
              (let [[call-id item-id] (str/split (or (:id block) "") #"\|" 2)
                    item-id           (if (and different-model?
                                               (string? item-id)
                                               (str/starts-with? item-id "fc_"))
                                        nil
                                        item-id)]
                [{:type      "function_call"
                  :id        item-id
                  :call_id   call-id
                  :name      (:name block)
                  :arguments ((:json/encode env) (or (:arguments block) {}))}])

              [])))
         vec)))

(defn- tool-result-output-items
  [model content]
  (let [text-output  (->> content
                          (filter #(= :text (:type %)))
                          (map :text)
                          (remove nil?)
                          (str/join "\n"))
        image-items  (when (contains? (get-in model [:capabilities :input] #{}) :image)
                       (->> content
                            (filter #(= :image (:type %)))
                            (mapv (partial block->input-user model))))
        output-items (cond-> []
                       (seq text-output)
                       (conj {:type "input_text" :text text-output})

                       (seq image-items)
                       (into image-items))]
    {:text-output  text-output
     :output-items output-items}))

(defn- convert-messages
  [env model context]
  (let [system-role (if (true? (get-in model [:capabilities :reasoning?])) "developer" "system")]
    (loop [remaining (:messages context)
           out       (cond-> []
                       (seq (:system-prompt context))
                       (conj {:role system-role :content (:system-prompt context)}))
           idx       0]
      (if-let [message (first remaining)]
        (case (:role message)
          :user
          (let [content (:content message)]
            (if (string? content)
              (recur (rest remaining)
                     (conj out {:role "user" :content [{:type "input_text" :text content}]})
                     (inc idx))
              (let [parts (->> content
                               (map (partial block->input-user model))
                               (remove nil?)
                               vec)]
                (recur (rest remaining)
                       (cond-> out
                         (seq parts) (conj {:role "user" :content parts}))
                       (inc idx)))))

          :assistant
          (recur (rest remaining)
                 (into out (assistant-blocks->responses-items env model message idx))
                 (inc idx))

          :tool-result
          (let [{:keys [text-output output-items]} (tool-result-output-items model (:content message))
                [call-id]                          (str/split (:tool-call-id message) #"\|" 2)
                out                                (conj out {:type    "function_call_output"
                                                              :call_id call-id
                                                              :output  (if (seq output-items)
                                                                         output-items
                                                                         text-output)})]
            (recur (rest remaining) out (inc idx)))

          (recur (rest remaining) out (inc idx)))
        out))))

(defn- convert-tools
  [tools]
  (->> tools
       (mapv (fn [tool]
               {:type        "function"
                :name        (:name tool)
                :description (:description tool)
                :parameters  (schema/malli->json-schema (or (:input-schema tool) {}))
                :strict      false}))))

(>defn build-request
       ([env model context opts]
        [:ol.llx/env :ol.llx/model :ol.llx/context-map :ol.llx/openai-responses-provider-options => :ol.llx/adapter-request-map]
        (build-request env model context opts false))
       ([env model context opts stream?]
        [:ol.llx/env :ol.llx/model :ol.llx/context-map :ol.llx/openai-responses-provider-options :boolean => :ol.llx/adapter-request-map]
        (let [api-key         (or (:api-key opts)
                                  (when-let [env-get (:env/get env)]
                                    (env-get "OPENAI_API_KEY")))
              _               (when-not (seq api-key)
                                (throw (ex-info "OpenAI API key is required" {:provider (:provider model)})))
              input           (convert-messages env model context)
              cache           (normalize-cache-control env (:cache-control opts))
              cache-key       (when-not (= :none cache) (:session-id opts))
              cache-retention (prompt-cache-retention (:base-url model) cache)
              payload         (cond-> {:model  (:id model)
                                       :input  input
                                       :stream stream?
                                       :store  false}
                                cache-key       (assoc :prompt_cache_key cache-key)
                                cache-retention (assoc :prompt_cache_retention cache-retention)
                                (:max-output-tokens opts) (assoc :max_output_tokens (:max-output-tokens opts))
                                (contains? opts :temperature) (assoc :temperature (:temperature opts))
                                (seq (or (:tools context) (:tools opts))) (assoc :tools (convert-tools (or (:tools context) (:tools opts))))
                                (and (true? (get-in model [:capabilities :reasoning?]))
                                     (map? (:reasoning opts)))
                                (assoc :reasoning {:effort  (name (or (get-in opts [:reasoning :effort]) :medium))
                                                   :summary (name (or (get-in opts [:reasoning :summary]) :auto))}
                                       :include ["reasoning.encrypted_content"])

                                (and (true? (get-in model [:capabilities :reasoning?]))
                                     (not (map? (:reasoning opts)))
                                     (str/starts-with? (:id model) "gpt-5"))
                                (update :input conj {:role    "developer"
                                                     :content [{:type "input_text"
                                                                :text "# Juice: 0 !important"}]}))
              sanitized       ((:unicode/sanitize-payload env) payload)
              body            ((:json/encode env) sanitized)
              headers         (cond-> {"Content-Type"  "application/json"
                                       "Authorization" (str "Bearer " api-key)}
                                (:headers model) (merge (:headers model))
                                (:headers opts) (merge (:headers opts)))]
          (trove/log! {:level :trace
                       :id    :ol.llx.obs/provider-payload
                       :data  {:call-id  (:call/id env)
                               :provider (:provider model)
                               :api      (:api model)
                               :model-id (:id model)
                               :stream?  stream?
                               :url      (str (adapter-common/trim-trailing-slash (:base-url model)) "/responses")
                               :payload  sanitized}})
          {:method  :post
           :url     (str (adapter-common/trim-trailing-slash (:base-url model)) "/responses")
           :headers headers
           :body    body
           :as      (if stream? :stream :string)
           :throw   false})))

(defn- ensure-stream-state
  [env state]
  (if (:assistant-message state)
    state
    (let [model (:model state)]
      {:model             model
       :assistant-message {:role        :assistant
                           :content     []
                           :api         (:api model)
                           :provider    (:provider model)
                           :model       (:id model)
                           :usage       (adapter-common/empty-usage)
                           :stop-reason :stop
                           :timestamp   ((:clock/now-ms env))}
       :current-block     nil
       :current-item      nil})))

(defn- append-content-block
  [state block]
  (let [idx (count (get-in state [:assistant-message :content]))]
    [(-> state
         (update-in [:assistant-message :content] conj block))
     idx]))

(>defn decode-event
       [env state raw-chunk]
       [:ol.llx/env :ol.llx/runtime-stream-state :ol.llx/raw-stream-chunk => :ol.llx/runtime-decode-event-result]
       (let [state (ensure-stream-state env state)
             chunk (cond
                     (map? raw-chunk) raw-chunk
                     (string? raw-chunk) (adapter-common/parse-json-lenient env raw-chunk)
                     :else {})]
         (case (:type chunk)
           "response.output_item.added"
           (let [item (:item chunk)]
             (case (:type item)
               "reasoning"
               (let [[next-state idx] (append-content-block state {:type :thinking :thinking ""})]
                 {:state  (assoc next-state
                                 :current-item item
                                 :current-block {:kind :thinking :index idx})
                  :events [{:type :thinking-start}]})

               "message"
               (let [[next-state idx] (append-content-block state {:type :text :text ""})]
                 {:state  (assoc next-state
                                 :current-item item
                                 :current-block {:kind :text :index idx})
                  :events [{:type :text-start}]})

               "function_call"
               (let [id               (str (:call_id item) "|" (:id item))
                     [next-state idx] (append-content-block state {:type      :tool-call
                                                                   :id        id
                                                                   :name      (:name item)
                                                                   :arguments {}})]
                 {:state  (assoc next-state
                                 :current-item item
                                 :current-block {:kind         :tool-call
                                                 :index        idx
                                                 :id           id
                                                 :name         (:name item)
                                                 :partial-json (or (:arguments item) "")})
                  :events [{:type :toolcall-start :id id :name (:name item)}]})

               {:state state :events []}))

           "response.reasoning_summary_text.delta"
           (if (and (= "reasoning" (get-in state [:current-item :type]))
                    (= :thinking (get-in state [:current-block :kind])))
             (let [summary (vec (or (get-in state [:current-item :summary]) []))]
               (if-let [last-part (peek summary)]
                 (let [idx          (get-in state [:current-block :index])
                       delta        (:delta chunk "")
                       next-summary (assoc summary
                                           (dec (count summary))
                                           (update last-part :text (fnil str "") delta))
                       next-state   (-> state
                                        (assoc-in [:current-item :summary] next-summary)
                                        (update-in [:assistant-message :content idx :thinking] str delta))]
                   {:state  next-state
                    :events [{:type :thinking-delta :thinking delta}]})
                 {:state state :events []}))
             {:state state :events []})

           ;; No-op: intermediate event for when a new summary part is added to the
           ;; reasoning item.
           "response.reasoning_summary_part.added"
           (if (= "reasoning" (get-in state [:current-item :type]))
             (let [part       (:part chunk)
                   next-state (update-in state [:current-item :summary] (fnil conj []) part)]
               {:state next-state :events []})
             {:state state :events []})

           "response.reasoning_summary_part.done"
           (if (and (= "reasoning" (get-in state [:current-item :type]))
                    (= :thinking (get-in state [:current-block :kind])))
             (let [summary (vec (or (get-in state [:current-item :summary]) []))]
               (if-let [last-part (peek summary)]
                 (let [idx          (get-in state [:current-block :index])
                       delta        "\n\n"
                       next-summary (assoc summary
                                           (dec (count summary))
                                           (update last-part :text (fnil str "") delta))
                       next-state   (-> state
                                        (assoc-in [:current-item :summary] next-summary)
                                        (update-in [:assistant-message :content idx :thinking] str delta))]
                   {:state  next-state
                    :events [{:type :thinking-delta :thinking delta}]})
                 {:state state :events []}))
             {:state state :events []})

           "response.output_text.delta"
           (if (= :text (get-in state [:current-block :kind]))
             (let [idx        (get-in state [:current-block :index])
                   delta      (:delta chunk "")
                   next-state (update-in state [:assistant-message :content idx :text] str delta)]
               {:state  next-state
                :events [{:type :text-delta :text delta}]})
             {:state state :events []})

           "response.refusal.delta"
           (if (= :text (get-in state [:current-block :kind]))
             (let [idx        (get-in state [:current-block :index])
                   delta      (:delta chunk "")
                   next-state (update-in state [:assistant-message :content idx :text] str delta)]
               {:state  next-state
                :events [{:type :text-delta :text delta}]})
             {:state state :events []})

           "response.function_call_arguments.delta"
           (if (= :tool-call (get-in state [:current-block :kind]))
             (let [delta        (:delta chunk "")
                   partial-json (str (get-in state [:current-block :partial-json] "") delta)
                   args         (adapter-common/parse-json-safe env partial-json)
                   idx          (get-in state [:current-block :index])
                   id           (get-in state [:current-block :id])
                   name         (get-in state [:current-block :name])
                   next-state   (-> state
                                    (assoc-in [:current-block :partial-json] partial-json)
                                    (assoc-in [:assistant-message :content idx :arguments] args))]
               {:state  next-state
                :events [{:type :toolcall-delta :id id :name name :arguments args}]})
             {:state state :events []})

           "response.function_call_arguments.done"
           (if (= :tool-call (get-in state [:current-block :kind]))
             (let [partial-json (:arguments chunk "")
                   args         (adapter-common/parse-json-lenient env partial-json)
                   idx          (get-in state [:current-block :index])
                   next-state   (-> state
                                    (assoc-in [:current-block :partial-json] partial-json)
                                    (assoc-in [:assistant-message :content idx :arguments] args))]
               {:state next-state :events []})
             {:state state :events []})

           "response.output_item.done"
           (let [item (:item chunk)]
             (case (:type item)
               "reasoning"
               (if (= :thinking (get-in state [:current-block :kind]))
                 (let [idx        (get-in state [:current-block :index])
                       thinking   (->> (:summary item)
                                       (map :text)
                                       (remove nil?)
                                       (str/join "\n\n"))
                       next-state (-> state
                                      (assoc-in [:assistant-message :content idx :thinking] (or thinking ""))
                                      (assoc-in [:assistant-message :content idx :signature] ((:json/encode env) item))
                                      (assoc :current-item nil :current-block nil))]
                   {:state  next-state
                    :events [{:type :thinking-end}]})
                 {:state state :events []})

               "message"
               (if (= :text (get-in state [:current-block :kind]))
                 (let [idx        (get-in state [:current-block :index])
                       text       (->> (:content item)
                                       (map (fn [part]
                                              (case (:type part)
                                                "output_text" (:text part)
                                                "refusal" (:refusal part)
                                                nil)))
                                       (remove nil?)
                                       (apply str))
                       next-state (-> state
                                      (assoc-in [:assistant-message :content idx :text] (or text ""))
                                      (assoc-in [:assistant-message :content idx :signature]
                                                (encode-text-signature-v1 env (:id item) (:phase item)))
                                      (assoc :current-item nil :current-block nil))]
                   {:state  next-state
                    :events [{:type :text-end}]})
                 {:state state :events []})

               "function_call"
               (if (= :tool-call (get-in state [:current-block :kind]))
                 (let [idx        (get-in state [:current-block :index])
                       id         (or (get-in state [:current-block :id])
                                      (str (:call_id item) "|" (:id item)))
                       name       (or (get-in state [:current-block :name]) (:name item))
                       args-json  (or (get-in state [:current-block :partial-json]) (:arguments item) "{}")
                       args       (adapter-common/parse-json-lenient env args-json)
                       next-state (-> state
                                      (assoc-in [:assistant-message :content idx :id] id)
                                      (assoc-in [:assistant-message :content idx :name] name)
                                      (assoc-in [:assistant-message :content idx :arguments] args)
                                      (assoc :current-item nil :current-block nil))]
                   {:state  next-state
                    :events [{:type :toolcall-end :id id :name name :arguments args}]})
                 {:state state :events []})

               {:state state :events []}))

           "response.completed"
           (let [response   (:response chunk)
                 next-state (-> state
                                (assoc-in [:assistant-message :usage]
                                          (usage->canonical
                                           (:model state)
                                           (:usage response)
                                           (:service_tier response)))
                                (assoc-in [:assistant-message :stop-reason] (map-stop-reason (:status response))))
                 next-state (if (and (= :stop (get-in next-state [:assistant-message :stop-reason]))
                                     (some #(= :tool-call (:type %)) (get-in next-state [:assistant-message :content])))
                              (assoc-in next-state [:assistant-message :stop-reason] :tool-use)
                              next-state)]
             {:state next-state :events []})

           "response.failed"
           (let [message (response-failed-message chunk)]
             (throw (ex-info message {:error message :chunk chunk})))

           "error"
           (throw (ex-info "OpenAI responses stream error"
                           {:error (str "Error Code " (:code chunk) ": " (:message chunk))}))

           {:state state :events []})))

(defn- output-item->canonical-content
  [env item]
  (case (:type item)
    "reasoning"
    [(cond-> {:type     :thinking
              :thinking (->> (:summary item)
                             (map :text)
                             (remove nil?)
                             (str/join "\n\n"))}
       (map? item) (assoc :signature ((:json/encode env) item)))]

    "message"
    (let [text (->> (:content item)
                    (map (fn [part]
                           (case (:type part)
                             "output_text" (:text part)
                             "refusal" (:refusal part)
                             nil)))
                    (remove nil?)
                    (apply str))]
      (if (seq text)
        [(cond-> {:type :text :text text}
           (:id item) (assoc :signature (encode-text-signature-v1 env (:id item) (:phase item))))]
        []))

    "function_call"
    [{:type      :tool-call
      :id        (str (:call_id item) "|" (:id item))
      :name      (:name item)
      :arguments (adapter-common/parse-json-lenient env (or (:arguments item) "{}"))}]

    []))

(defn- response->assistant-message
  [env model response]
  (let [body   (cond
                 (map? (:body response)) (:body response)
                 (string? (:body response)) ((:json/decode env) (:body response) {:key-fn keyword})
                 :else {})
        status (long (or (:status response) 0))]
    (when (or (< status 200) (>= status 300))
      (throw
       (ex-info "OpenAI responses request failed"
                {:status status
                 :error  (or (get-in body [:error :message]) body)})))
    (let [content     (->> (:output body)
                           (mapcat (partial output-item->canonical-content env))
                           vec)
          stop-reason (map-stop-reason (:status body))
          stop-reason (if (and (= :stop stop-reason)
                               (some #(= :tool-call (:type %)) content))
                        :tool-use
                        stop-reason)]
      {:role        :assistant
       :content     content
       :api         (:api model)
       :provider    (:provider model)
       :model       (:id model)
       :usage       (usage->canonical model (:usage body) (:service_tier body))
       :stop-reason stop-reason
       :timestamp   ((:clock/now-ms env))})))

(>defn finalize
       [env state-or-response]
       [:ol.llx/env :ol.llx/runtime-finalize-input => :ol.llx/runtime-finalize-result]
       (if (contains? state-or-response :response)
         {:assistant-message (response->assistant-message env (:model state-or-response) (:response state-or-response))
          :events            []}
         (let [state-or-response (ensure-stream-state env state-or-response)]
           {:assistant-message (:assistant-message state-or-response)
            :events            []})))

(>defn normalize-error
       [env ex partial-state]
       [:ol.llx/env any? :ol.llx/runtime-normalize-error-partial => :ol.llx/message-assistant]
       (let [model             (or (:model partial-state) (-> ex ex-data :model))
             assistant-message (or (:assistant-message partial-state)
                                   (:assistant-message (ensure-stream-state env {:model model}))
                                   {:role        :assistant
                                    :content     []
                                    :api         (:api model)
                                    :provider    (:provider model)
                                    :model       (:id model)
                                    :usage       (adapter-common/empty-usage)
                                    :stop-reason :error
                                    :timestamp   ((:clock/now-ms env))})
             aborted?          (or (:aborted? (ex-data ex))
                                   (= "Request was aborted" (ex-message ex)))
             error-detail      (some-> (ex-data ex) :error str)
             error-message     (or (when (seq (or error-detail ""))
                                     (if (and (ex-message ex) (not (str/includes? (ex-message ex) error-detail)))
                                       (str (ex-message ex) ": " error-detail)
                                       error-detail))
                                   (ex-message ex)
                                   (pr-str ex))]
         (-> assistant-message
             (assoc :stop-reason (if aborted? :aborted :error))
             (assoc :error-message error-message))))

(>defn handle-open-stream-response
       [env model response]
       [:ol.llx/env :ol.llx/model :ol.llx/http-response-map => :ol.llx/http-response-map]
       (let [response (schema/assert-valid! :ol.llx/http-response-map response)
             status   (long (or (:status response) 0))
             provider (name (or (:provider model) "unknown"))]
         (when (or (< status 200) (>= status 300))
           (let [body-string   (cond
                                 (string? (:body response)) (:body response)
                                 (nil? (:body response)) ""
                                 :else (if-let [read-body-string (:http/read-body-string env)]
                                         (read-body-string (:body response))
                                         ""))
                 body          (adapter-common/parse-json-safe env body-string)
                 headers       (:headers response)
                 message       (or (get-in body [:error :message]) body-string)
                 provider-code (get-in body [:error :type])
                 request-id    (get headers "x-request-id")
                 retry-after   (errors/extract-retry-after-hint headers message)]
             (throw (errors/http-status->error
                     status provider message
                     :provider-code provider-code
                     :retry-after retry-after
                     :request-id request-id
                     :body body))))
         response))

(>defn open-stream
       [env _model request-map]
       [:ol.llx/env :ol.llx/model :ol.llx/adapter-request-map => :ol.llx/deferred]
       (-> ((:http/request env) request-map)
           (p/then (fn [response]
                     (handle-open-stream-response env _model response)))))

(defn adapter
  []
  {:api                    :openai-responses
   :build-request          build-request
   :open-stream            open-stream
   :decode-event           decode-event
   :finalize               finalize
   :normalize-error        normalize-error
   :supports-model?        (fn [model] (= :openai-responses (:api model)))
   :normalize-tool-call-id normalize-tool-call-id
   :transform-options      {:id-normalization-profile :openai-responses}
   :transform-context      (fn [_model context] context)})
