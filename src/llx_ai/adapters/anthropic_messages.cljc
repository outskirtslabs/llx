(ns llx-ai.adapters.anthropic-messages
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [clojure.string :as str]
   [llx-ai.errors :as errors]
   [llx-ai.models :as models]
   [llx-ai.schema :as schema]
   [taoensso.trove :as trove]))

(defn- trim-trailing-slash
  [s]
  (if (and (string? s)
           (pos? (count s))
           (= \/ (nth s (dec (count s)))))
    (subs s 0 (dec (count s)))
    s))

(defn- parse-json-safe
  [env s]
  (if-let [decode-safe-fn (:json/decode-safe env)]
    (or (decode-safe-fn s {:key-fn keyword}) {})
    {}))

(defn- parse-json-lenient
  [env s]
  (if-let [decode-safe-fn (:json/decode-safe env)]
    (or (decode-safe-fn s {:key-fn keyword})
        ((:json/decode env) s {:key-fn keyword})
        {})
    ((:json/decode env) s {:key-fn keyword})))

(defn- empty-usage
  []
  {:input        0
   :output       0
   :cache-read   0
   :cache-write  0
   :total-tokens 0
   :cost         {:input       0.0
                  :output      0.0
                  :cache-read  0.0
                  :cache-write 0.0
                  :total       0.0}})

(defn- usage->canonical
  [model usage]
  (let [input       (long (or (:input_tokens usage) 0))
        output      (long (or (:output_tokens usage) 0))
        cache-read  (long (or (:cache_read_input_tokens usage) 0))
        cache-write (long (or (:cache_creation_input_tokens usage) 0))
        total       (+ input output cache-read cache-write)
        usage*      {:input        input
                     :output       output
                     :cache-read   cache-read
                     :cache-write  cache-write
                     :total-tokens total}]
    {:input        input
     :output       output
     :cache-read   cache-read
     :cache-write  cache-write
     :total-tokens total
     :cost         (models/calculate-cost model usage*)}))

(defn- map-stop-reason
  [stop-reason]
  (case stop-reason
    "end_turn" :stop
    "max_tokens" :length
    "tool_use" :tool-use
    "pause_turn" :stop
    "stop_sequence" :stop
    "refusal" :error
    "sensitive" :error
    (throw (ex-info "Unknown Anthropic stop reason" {:stop-reason stop-reason}))))

(defn normalize-tool-call-id
  [tool-call-id _target-model _source-assistant-message]
  (let [sanitized (-> (or tool-call-id "")
                      (str/replace #"[^A-Za-z0-9_-]" "_"))]
    (if (> (count sanitized) 64)
      (subs sanitized 0 64)
      sanitized)))

(defn- convert-image-block
  [block]
  {:type   "image"
   :source {:type       "base64"
            :media_type (:mime-type block)
            :data       (:data block)}})

(defn- convert-text-or-image-blocks
  [blocks]
  (->> blocks
       (map (fn [block]
              (case (:type block)
                :text (let [text (:text block)]
                        (when (and (string? text) (not (str/blank? text)))
                          {:type "text" :text text}))
                :image (convert-image-block block)
                nil)))
       (remove nil?)
       vec))

(defn- blocks->anthropic-content
  [content]
  (if (string? content)
    content
    (let [converted (convert-text-or-image-blocks content)]
      (if (every? #(= "text" (:type %)) converted)
        (str/join "\n" (map :text converted))
        converted))))

(defn- convert-user-message
  [model message]
  (let [content (:content message)]
    (if (string? content)
      (when-not (str/blank? content)
        {:role "user" :content content})
      (let [supports-image? (contains? (get-in model [:capabilities :input] #{}) :image)
            blocks          (->> content
                                 (remove (fn [block]
                                           (and (= :image (:type block))
                                                (not supports-image?))))
                                 convert-text-or-image-blocks)]
        (when (seq blocks)
          {:role "user" :content blocks})))))

(defn- convert-assistant-block
  [block]
  (case (:type block)
    :text (let [text (:text block)]
            (when (and (string? text) (not (str/blank? text)))
              {:type "text" :text text}))
    :thinking (let [thinking  (:thinking block)
                    signature (:signature block)]
                (when (and (string? thinking) (not (str/blank? thinking)))
                  (if (and (string? signature) (not (str/blank? signature)))
                    {:type "thinking" :thinking thinking :signature signature}
                    {:type "text" :text thinking})))
    :tool-call {:type  "tool_use"
                :id    (:id block)
                :name  (:name block)
                :input (or (:arguments block) {})}
    nil))

(defn- convert-assistant-message
  [message]
  (let [blocks (->> (:content message)
                    (map convert-assistant-block)
                    (remove nil?)
                    vec)]
    (when (seq blocks)
      {:role "assistant" :content blocks})))

(defn- convert-tool-result
  [message]
  {:type        "tool_result"
   :tool_use_id (:tool-call-id message)
   :content     (blocks->anthropic-content (:content message))
   :is_error    (:is-error? message)})

(defn- convert-messages
  [model context]
  (loop [remaining (:messages context)
         out       []]
    (if-let [message (first remaining)]
      (case (:role message)
        :user
        (recur (rest remaining)
               (cond-> out
                 (convert-user-message model message) (conj (convert-user-message model message))))

        :assistant
        (recur (rest remaining)
               (cond-> out
                 (convert-assistant-message message) (conj (convert-assistant-message message))))

        :tool-result
        (let [[tool-results rest-messages] (split-with #(= :tool-result (:role %)) remaining)
              converted                    (mapv convert-tool-result tool-results)]
          (recur rest-messages
                 (conj out {:role "user" :content converted})))

        (recur (rest remaining) out))
      out)))

(defn- supports-adaptive-thinking?
  [model-id]
  (or (str/includes? (or model-id "") "opus-4-6")
      (str/includes? (or model-id "") "opus-4.6")))

(defn- map-reasoning-level->effort
  [level]
  (case level
    :minimal "low"
    :low     "low"
    :medium  "medium"
    :high    "high"
    :xhigh   "max"
    "high"))

(defn- adjust-max-tokens-for-thinking
  [base-max-tokens model-max-tokens reasoning-level]
  (let [budgets         {:minimal 1024 :low 2048 :medium 8192 :high 16384}
        level           (if (= :xhigh reasoning-level) :high reasoning-level)
        thinking-budget (get budgets level 8192)
        max-tokens      (min (+ base-max-tokens thinking-budget) model-max-tokens)
        min-output      1024
        thinking-budget (if (<= max-tokens thinking-budget)
                          (max 0 (- max-tokens min-output))
                          thinking-budget)]
    {:max-tokens max-tokens :thinking-budget thinking-budget}))

(defn- convert-tools
  [tools]
  (->> tools
       (map (fn [tool]
              {:name         (:name tool)
               :description  (:description tool)
               :input_schema (schema/malli->json-schema (or (:input-schema tool) {}))}))
       vec))

(>defn build-request
       ([env model context opts]
        [:llx/env :llx/model :llx/context-map :llx/request-options => :llx/adapter-request-map]
        (build-request env model context opts false))
       ([env model context opts stream?]
        [:llx/env :llx/model :llx/context-map :llx/request-options :boolean => :llx/adapter-request-map]
        (let [api-key          (or (:api-key opts)
                                   (when-let [env-get (:env/get env)]
                                     (env-get "ANTHROPIC_API_KEY")))
              _                (when-not (seq api-key)
                                 (throw (ex-info "Anthropic API key is required" {:provider (:provider model)})))
              reasoning-level  (get-in opts [:reasoning :level])
              reasoning-model? (true? (get-in model [:capabilities :reasoning?]))
              adaptive?        (and reasoning-level reasoning-model?
                                    (supports-adaptive-thinking? (:id model)))
              budget?          (and reasoning-level reasoning-model?
                                    (not adaptive?))
              base-max         (or (:max-output-tokens opts)
                                   (max 1 (long (/ (:max-tokens model) 3))))
              adjusted         (when budget?
                                 (adjust-max-tokens-for-thinking
                                  base-max (:max-tokens model) reasoning-level))
              payload          (cond-> {:model      (:id model)
                                        :messages   (convert-messages model context)
                                        :max_tokens (if adjusted
                                                      (:max-tokens adjusted)
                                                      base-max)
                                        :stream     stream?}
                                 (seq (:system-prompt context))
                                 (assoc :system [{:type "text" :text (:system-prompt context)}])

                                 (contains? opts :temperature)
                                 (assoc :temperature (:temperature opts))

                                 (seq (:tools context))
                                 (assoc :tools (convert-tools (:tools context)))

                                 (seq (:tools opts))
                                 (assoc :tools (convert-tools (:tools opts)))

                                 (:tool-choice opts)
                                 (assoc :tool_choice (if (keyword? (:tool-choice opts))
                                                       {:type (name (:tool-choice opts))}
                                                       {:type (:tool-choice opts)}))

                                 adaptive?
                                 (assoc :thinking {:type "adaptive"}
                                        :output_config {:effort (map-reasoning-level->effort reasoning-level)})

                                 budget?
                                 (assoc :thinking {:type          "enabled"
                                                   :budget_tokens (:thinking-budget adjusted)}))
              base-url         (trim-trailing-slash (:base-url model))
              headers          (cond-> {"Content-Type"      "application/json"
                                        "anthropic-version" "2023-06-01"
                                        "x-api-key"         api-key}
                                 (:headers model) (merge (:headers model))
                                 (:headers opts) (merge (:headers opts)))
              sanitized        ((:unicode/sanitize-payload env) payload)]
          (trove/log! {:level :trace
                       :id    :llx.obs/provider-payload
                       :data  {:call-id  (:call/id env)
                               :provider (:provider model)
                               :api      (:api model)
                               :model-id (:id model)
                               :stream?  stream?
                               :url      (str base-url "/v1/messages")
                               :payload  sanitized}})
          {:method  :post
           :url     (str base-url "/v1/messages")
           :headers headers
           :body    ((:json/encode env) sanitized)
           :as      (if stream? :stream :string)
           :throw   false})))

(defn- decode-response-body
  [env body]
  (cond
    (map? body) body
    (string? body) ((:json/decode env) body {:key-fn keyword})
    (nil? body) {}
    :else (if-let [read-body-string (:http/read-body-string env)]
            ((:json/decode env) (read-body-string body) {:key-fn keyword})
            {})))

(defn- response-content->canonical
  [content]
  (->> content
       (map (fn [block]
              (case (:type block)
                "text" {:type :text :text (:text block)}
                "thinking" (cond-> {:type :thinking :thinking (:thinking block)}
                             (seq (:signature block)) (assoc :signature (:signature block)))
                "tool_use" {:type      :tool-call
                            :id        (:id block)
                            :name      (:name block)
                            :arguments (or (:input block) {})}
                nil)))
       (remove nil?)
       vec))

(>defn finalize
       [env state-or-response]
       [:llx/env :llx/runtime-finalize-input => :llx/runtime-finalize-result]
       (if (contains? state-or-response :response)
         (let [response (:response state-or-response)
               body     (decode-response-body env (:body response))
               status   (long (or (:status response) 0))]
           (when (or (< status 200) (>= status 300))
             (throw
              (ex-info "Anthropic messages request failed"
                       {:status status
                        :error  (or (get-in body [:error :message]) body)})))
           {:assistant-message {:role        :assistant
                                :content     (response-content->canonical (:content body))
                                :api         (get-in state-or-response [:model :api])
                                :provider    (get-in state-or-response [:model :provider])
                                :model       (get-in state-or-response [:model :id])
                                :usage       (usage->canonical (get-in state-or-response [:model]) (:usage body))
                                :stop-reason (map-stop-reason (:stop_reason body))
                                :timestamp   ((:clock/now-ms env))}
            :events            []})
         {:assistant-message (:assistant-message state-or-response)
          :events            []}))

(defn- init-stream-state
  [env model]
  {:model             model
   :assistant-message {:role        :assistant
                       :content     []
                       :api         (:api model)
                       :provider    (:provider model)
                       :model       (:id model)
                       :usage       (empty-usage)
                       :stop-reason :stop
                       :timestamp   ((:clock/now-ms env))}
   :stream-blocks     {}})

(defn- append-content-block
  [state block]
  (let [content-index (count (get-in state [:assistant-message :content]))]
    {:state         (update-in state [:assistant-message :content] conj block)
     :content-index content-index}))

(defn- update-usage
  [state usage]
  (if (map? usage)
    (assoc-in state [:assistant-message :usage] (usage->canonical (:model state) usage))
    state))

(>defn decode-event
       [env state raw-chunk]
       [:llx/env :llx/runtime-stream-state :llx/raw-stream-chunk => :llx/runtime-decode-event-result]
       (let [state (if (:assistant-message state)
                     state
                     (init-stream-state env (:model state)))
             chunk (cond
                     (map? raw-chunk) raw-chunk
                     (string? raw-chunk) (parse-json-lenient env raw-chunk)
                     :else {})]
         (case (:type chunk)
           "message_start"
           {:state  (update-usage state (get-in chunk [:message :usage]))
            :events []}

           "content_block_start"
           (let [index (:index chunk)
                 block (:content_block chunk)]
             (case (:type block)
               "text"
               (let [{next-state :state content-index :content-index}
                     (append-content-block state {:type :text :text ""})]
                 {:state  (assoc-in next-state [:stream-blocks index]
                                    {:kind :text :content-index content-index})
                  :events [{:type :text-start}]})

               "thinking"
               (let [{next-state :state content-index :content-index}
                     (append-content-block state {:type :thinking :thinking ""})]
                 {:state  (assoc-in next-state [:stream-blocks index]
                                    {:kind :thinking :content-index content-index})
                  :events [{:type :thinking-start}]})

               "tool_use"
               (let [{next-state :state content-index :content-index}
                     (append-content-block state {:type      :tool-call
                                                  :id        (:id block)
                                                  :name      (:name block)
                                                  :arguments (or (:input block) {})})]
                 {:state  (assoc-in next-state [:stream-blocks index]
                                    {:kind          :tool-call
                                     :content-index content-index
                                     :id            (:id block)
                                     :name          (:name block)
                                     :partial-json  ""})
                  :events [{:type :toolcall-start :id (:id block) :name (:name block)}]})

               {:state state :events []}))

           "content_block_delta"
           (let [index         (:index chunk)
                 stream-block  (get-in state [:stream-blocks index])
                 content-index (:content-index stream-block)
                 current-id    (or (:id stream-block) (get-in state [:assistant-message :content content-index :id]))
                 current-name  (or (:name stream-block) (get-in state [:assistant-message :content content-index :name]))]
             (case (get-in chunk [:delta :type])
               "text_delta"
               (let [delta (get-in chunk [:delta :text] "")]
                 {:state  (update-in state [:assistant-message :content content-index :text] str delta)
                  :events [{:type :text-delta :text delta}]})

               "thinking_delta"
               (let [delta (get-in chunk [:delta :thinking] "")]
                 {:state  (update-in state [:assistant-message :content content-index :thinking] str delta)
                  :events [{:type :thinking-delta :thinking delta}]})

               "signature_delta"
               (let [delta (get-in chunk [:delta :signature] "")]
                 {:state  (update-in state [:assistant-message :content content-index :signature] (fnil str "") delta)
                  :events []})

               "input_json_delta"
               (let [delta        (get-in chunk [:delta :partial_json] "")
                     partial-json (str (or (:partial-json stream-block) "") delta)
                     parsed-args  (parse-json-safe env partial-json)
                     next-state   (-> state
                                      (assoc-in [:stream-blocks index :partial-json] partial-json)
                                      (assoc-in [:assistant-message :content content-index :arguments] parsed-args))]
                 {:state  next-state
                  :events [{:type :toolcall-delta :id current-id :name current-name :arguments parsed-args}]})

               {:state state :events []}))

           "content_block_stop"
           (let [index         (:index chunk)
                 stream-block  (get-in state [:stream-blocks index])
                 content-index (:content-index stream-block)
                 tool-call     (get-in state [:assistant-message :content content-index])]
             (case (:kind stream-block)
               :text {:state  (update state :stream-blocks dissoc index)
                      :events [{:type :text-end}]}
               :thinking {:state  (update state :stream-blocks dissoc index)
                          :events [{:type :thinking-end}]}
               :tool-call {:state  (update state :stream-blocks dissoc index)
                           :events [{:type      :toolcall-end
                                     :id        (:id tool-call)
                                     :name      (:name tool-call)
                                     :arguments (:arguments tool-call)}]}
               {:state state :events []}))

           "message_delta"
           (let [next-state (-> state
                                (assoc-in [:assistant-message :stop-reason]
                                          (map-stop-reason (get-in chunk [:delta :stop_reason])))
                                (update-usage (:usage chunk)))]
             {:state next-state :events []})

           {:state state :events []})))

(>defn normalize-error
       [env ex partial-state]
       [:llx/env any? :llx/runtime-normalize-error-partial => :llx/message-assistant]
       (let [model             (or (:model partial-state) (-> ex ex-data :model))
             assistant-message (or (:assistant-message partial-state)
                                   (:assistant-message (init-stream-state env model))
                                   {:role        :assistant
                                    :content     []
                                    :api         (:api model)
                                    :provider    (:provider model)
                                    :model       (:id model)
                                    :usage       (empty-usage)
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

(>defn open-stream
       [env _model request-map]
       [:llx/env :llx/model :llx/adapter-request-map => :llx/http-response-map]
       (let [response ((:http/request env) request-map)
             status   (long (or (:status response) 0))]
         (when (or (< status 200) (>= status 300))
           (let [body-string   (cond
                                 (string? (:body response)) (:body response)
                                 (nil? (:body response)) ""
                                 :else (if-let [read-body-string (:http/read-body-string env)]
                                         (read-body-string (:body response))
                                         ""))
                 body          (parse-json-safe env body-string)
                 headers       (:headers response)
                 message       (or (get-in body [:error :message]) body-string)
                 provider-code (get-in body [:error :type])
                 request-id    (get headers "x-request-id")
                 retry-after   (errors/extract-retry-after-hint headers message)
                 provider      (name (or (:provider _model) "unknown"))]
             (throw (errors/http-status->error
                     status provider message
                     :provider-code provider-code
                     :retry-after retry-after
                     :request-id request-id
                     :body body))))
         response))

(defn adapter
  []
  {:api                    :anthropic-messages
   :build-request          build-request
   :open-stream            open-stream
   :decode-event           decode-event
   :finalize               finalize
   :normalize-error        normalize-error
   :supports-model?        (fn [model] (= :anthropic-messages (:api model)))
   :normalize-tool-call-id normalize-tool-call-id
   :transform-options      {:id-normalization-profile :anthropic}
   :transform-context      (fn [_model context] context)})
