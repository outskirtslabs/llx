(ns llx-ai.adapters.openai-completions
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [clojure.string :as str]
   [llx-ai.errors :as errors]
   [llx-ai.schema :as schema]))
(schema/registry)

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

(defn- map-stop-reason
  [finish-reason]
  (case finish-reason
    "stop" :stop
    "length" :length
    "tool_calls" :tool-use
    "tool" :tool-use
    "content_filter" :error
    (throw (ex-info "Unknown OpenAI completions stop reason" {:finish-reason finish-reason}))))

(defn- usage->canonical
  [usage]
  (let [cached    (long (or (get-in usage [:prompt_tokens_details :cached_tokens]) 0))
        prompt    (long (or (:prompt_tokens usage) 0))
        reasoning (long (or (get-in usage [:completion_tokens_details :reasoning_tokens]) 0))
        output    (+ (long (or (:completion_tokens usage) 0)) reasoning)
        input     (max 0 (- prompt cached))
        total     (long (or (:total_tokens usage) (+ input output cached)))]
    {:input        input
     :output       output
     :cache-read   cached
     :cache-write  0
     :total-tokens total
     :cost         {:input       0.0
                    :output      0.0
                    :cache-read  0.0
                    :cache-write 0.0
                    :total       0.0}}))

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

(defn- convert-image-block
  [block]
  {:type      "image_url"
   :image_url {:url (str "data:" (:mime-type block) ";base64," (:data block))}})

(defn- detect-compat
  [model]
  (let [provider    (keyword (or (:provider model) :openai))
        base-url    (str/lower-case (or (:base-url model) ""))
        nonstandard (or (= provider :mistral)
                        (str/includes? base-url "mistral.ai")
                        (str/includes? base-url "chutes.ai"))
        mistral?    (or (= provider :mistral)
                        (str/includes? base-url "mistral.ai"))
        grok?       (or (= provider :xai) (str/includes? base-url "x.ai"))
        zai?        (or (= provider :zai) (str/includes? base-url "api.z.ai"))]
    {:store?                                (not nonstandard)
     :supports-usage-stream?                true
     :supports-strict-tools?                true
     :supports-reasoning-effort?            (not (or grok? zai?))
     :thinking-format                       (cond zai? :zai :else :openai)
     :token-field                           (if mistral? :max_tokens :max_completion_tokens)
     :requires-tool-result-name?            mistral?
     :requires-assistant-after-tool-result? false
     :requires-thinking-as-text?            mistral?}))

(defn- resolve-compat
  [model]
  (merge (detect-compat model) (:compat model)))

(defn- convert-user-content
  [model content]
  (if (string? content)
    content
    (let [supports-image? (contains? (get-in model [:capabilities :input] #{}) :image)
          blocks          (mapv (fn [block]
                                  (case (:type block)
                                    :text {:type "text" :text (:text block)}
                                    :image (when supports-image? (convert-image-block block))
                                    nil))
                                content)]
      (vec (remove nil? blocks)))))

(defn- assistant-content->string
  [content]
  (let [texts (->> content
                   (filter #(= :text (:type %)))
                   (map :text)
                   (remove nil?))]
    (when (seq texts)
      (apply str texts))))

(defn- tool-result-content->string
  [content]
  (->> content
       (filter #(= :text (:type %)))
       (map :text)
       (remove str/blank?)
       (str/join "\n")))

(defn- convert-message
  [env model message]
  (let [compat-profile (resolve-compat model)]
    (case (:role message)
      :user {:role    "user"
             :content (convert-user-content model (:content message))}
      :assistant (let [msg               {:role "assistant"}
                       base-text         (assistant-content->string (:content message))
                       thinking-blocks   (->> (:content message)
                                              (filter #(= :thinking (:type %)))
                                              (filter #(not (str/blank? (:thinking %)))))
                       thinking          (->> thinking-blocks
                                              (map :thinking)
                                              (str/join "\n\n"))
                       text-content      (if (and (:requires-thinking-as-text? compat-profile) (seq thinking))
                                           (if (seq base-text)
                                             (str thinking "\n\n" base-text)
                                             thinking)
                                           base-text)
                       tool-calls        (->> (:content message)
                                              (filter #(= :tool-call (:type %)))
                                              (mapv (fn [tool-call]
                                                      {:id       (:id tool-call)
                                                       :type     "function"
                                                       :function {:name      (:name tool-call)
                                                                  :arguments ((:json/encode env) (or (:arguments tool-call) {}))}})))
                       reasoning-details (->> (:content message)
                                              (filter #(= :tool-call (:type %)))
                                              (keep (fn [tc]
                                                      (when (:signature tc)
                                                        (parse-json-safe env (:signature tc)))))
                                              vec)
                       first-sig         (when (seq thinking-blocks)
                                           (:signature (first thinking-blocks)))
                       msg               (cond
                                           (seq tool-calls) (assoc msg :content (or text-content "") :tool_calls tool-calls)
                                           (some? text-content) (assoc msg :content text-content)
                                           :else nil)
                       msg               (when msg
                                           (cond-> msg
                                             (and (seq thinking-blocks)
                                                  (not (:requires-thinking-as-text? compat-profile))
                                                  (seq first-sig))
                                             (assoc (keyword first-sig)
                                                    (->> thinking-blocks (map :thinking) (str/join "\n")))

                                             (seq reasoning-details)
                                             (assoc :reasoning_details reasoning-details)))]
                   msg)
      :tool-result (cond-> {:role         "tool"
                            :tool_call_id (:tool-call-id message)
                            :content      (let [text (tool-result-content->string (:content message))]
                                            (if (seq text) text "(see attached image)"))}
                     (:requires-tool-result-name? compat-profile)
                     (assoc :name (:tool-name message)))
      nil)))

(defn- tool-result-images
  [model message]
  (when (contains? (get-in model [:capabilities :input] #{}) :image)
    (->> (:content message)
         (filter #(= :image (:type %)))
         (mapv convert-image-block))))

(defn- convert-messages
  [env model context]
  (let [system-prompt (:system-prompt context)
        out0          (cond-> []
                        (seq system-prompt)
                        (conj {:role "system" :content system-prompt}))]
    (loop [remaining (:messages context)
           out       out0]
      (if-let [message (first remaining)]
        (if (= :tool-result (:role message))
          (let [[batch rest-messages] (split-with #(= :tool-result (:role %)) remaining)
                out1                  (reduce (fn [acc tool-msg]
                                                (if-let [converted (convert-message env model tool-msg)]
                                                  (conj acc converted)
                                                  acc))
                                              out
                                              batch)
                image-blocks          (->> batch
                                           (mapcat #(tool-result-images model %))
                                           vec)
                out2                  (cond-> out1
                                        (seq image-blocks)
                                        (conj {:role    "user"
                                               :content (into [{:type "text"
                                                                :text "Attached image(s) from tool result:"}]
                                                              image-blocks)}))]
            (recur rest-messages out2))
          (recur (rest remaining)
                 (if-let [converted (convert-message env model message)]
                   (conj out converted)
                   out)))
        (vec out)))))

(defn- convert-tools
  [model tools]
  (let [compat-profile (resolve-compat model)]
    (when (seq tools)
      (mapv
       (fn [tool]
         {:type     "function"
          :function (cond-> {:name        (:name tool)
                             :description (:description tool)
                             :parameters  (schema/malli->json-schema (or (:input-schema tool) {}))}
                      (:supports-strict-tools? compat-profile)
                      (assoc :strict false))})
       tools))))

(defn- tool-choice->wire
  [tool-choice]
  (cond
    (keyword? tool-choice) (name tool-choice)
    (#{"auto" "none" "required"} tool-choice) tool-choice
    (string? tool-choice) {:type "function" :function {:name tool-choice}}
    :else tool-choice))

(defn- provider->env-var
  [provider]
  (case (keyword provider)
    :openai "OPENAI_API_KEY"
    :mistral "MISTRAL_API_KEY"
    nil))

(defn- env-api-key
  [env provider]
  (when-let [env-var (provider->env-var provider)]
    (when-let [env-get (:env/get env)]
      (env-get env-var))))

(defn- missing-api-key-message
  [provider]
  (if-let [env-var (provider->env-var provider)]
    (str "API key is required for provider " (pr-str provider)
         ". Set " env-var " or pass :api-key.")
    (str "API key is required for provider " (pr-str provider)
         ". Pass :api-key.")))

(>defn build-request
       ([env model context opts]
        [:llx/env :llx/model :llx/context-map :llx/request-options => :llx/adapter-request-map]
        (build-request env model context opts false))
       ([env model context opts stream?]
        [:llx/env :llx/model :llx/context-map :llx/request-options :boolean => :llx/adapter-request-map]
        (let [compat-profile   (resolve-compat model)
              api-key          (or (:api-key opts)
                                   (env-api-key env (:provider model)))
              needs-api-key?   (not= :openai-compatible (:provider model))
              _                (when (and needs-api-key? (not (seq api-key)))
                                 (throw (ex-info (missing-api-key-message (:provider model))
                                                 {:provider (:provider model)})))
              reasoning-level  (get-in opts [:reasoning :level])
              reasoning-model? (true? (get-in model [:capabilities :reasoning?]))
              thinking-format  (or (:thinking-format compat-profile) :openai)
              payload          (cond-> {:model    (:id model)
                                        :messages (convert-messages env model context)
                                        :stream   stream?}
                                 (and stream? (:supports-usage-stream? compat-profile))
                                 (assoc :stream_options {:include_usage true})

                                 (:store? compat-profile)
                                 (assoc :store false)

                                 (:max-output-tokens opts)
                                 (assoc (or (:token-field compat-profile) :max_completion_tokens)
                                        (:max-output-tokens opts))

                                 (and reasoning-level reasoning-model?
                                      (= :zai thinking-format))
                                 (assoc :thinking {:type "enabled"})

                                 (and (not reasoning-level) reasoning-model?
                                      (= :zai thinking-format))
                                 (assoc :thinking {:type "disabled"})

                                 (and reasoning-level reasoning-model?
                                      (= :qwen thinking-format))
                                 (assoc :enable_thinking true)

                                 (and reasoning-level reasoning-model?
                                      (not (#{:zai :qwen} thinking-format))
                                      (:supports-reasoning-effort? compat-profile))
                                 (assoc :reasoning_effort (name reasoning-level))

                                 (contains? opts :temperature) (assoc :temperature (:temperature opts))
                                 (contains? opts :top-p) (assoc :top_p (:top-p opts))
                                 (seq (or (:tools context) (:tools opts))) (assoc :tools (convert-tools model (or (:tools context) (:tools opts))))
                                 (:tool-choice opts) (assoc :tool_choice (tool-choice->wire (:tool-choice opts))))
              body             ((:json/encode env) payload)
              base-url         (trim-trailing-slash (:base-url model))
              headers          (cond-> {"Content-Type" "application/json"}
                                 (seq api-key) (assoc "Authorization" (str "Bearer " api-key))
                                 (:headers model) (merge (:headers model))
                                 (:headers opts) (merge (:headers opts)))]
          {:method  :post
           :url     (str base-url "/chat/completions")
           :headers headers
           :body    body
           :as      (if stream? :stream :string)
           :throw   false})))

(defn- message->canonical-content
  [env message]
  (let [tool-calls   (seq (:tool_calls message))
        text-content (:content message)]
    (cond
      tool-calls
      (mapv (fn [tc]
              {:type      :tool-call
               :id        (or (:id tc) "")
               :name      (get-in tc [:function :name] "")
               :arguments (parse-json-lenient env (or (get-in tc [:function :arguments]) "{}"))})
            tool-calls)

      (string? text-content)
      [{:type :text :text text-content}]

      (vector? text-content)
      (->> text-content
           (map (fn [part]
                  (case (:type part)
                    "text" {:type :text :text (:text part)}
                    nil)))
           (remove nil?)
           vec)

      :else
      [])))

(defn- decode-response-body
  [env body]
  (cond
    (map? body) body
    (string? body) ((:json/decode env) body {:key-fn keyword})
    (nil? body) {}
    :else (if-let [read-body-string (:http/read-body-string env)]
            ((:json/decode env) (read-body-string body) {:key-fn keyword})
            {})))

(defn response->assistant-message
  [env model response]
  (let [body   (decode-response-body env (:body response))
        status (long (or (:status response) 0))]
    (when (or (< status 200) (>= status 300))
      (throw
       (ex-info "OpenAI completions request failed"
                {:status status
                 :error  (or (get-in body [:error :message]) body)})))
    (let [choice  (first (:choices body))
          message (:message choice)]
      (when-not choice
        (throw (ex-info "OpenAI completions response missing choices" {:body body})))
      {:role        :assistant
       :content     (message->canonical-content env message)
       :api         (:api model)
       :provider    (:provider model)
       :model       (:id model)
       :usage       (usage->canonical (:usage body))
       :stop-reason (map-stop-reason (:finish_reason choice))
       :timestamp   ((:clock/now-ms env))})))

(defn complete*
  [env model context opts]
  (let [request  (build-request env model context opts)
        response ((:http/request env) request)]
    (response->assistant-message env model response)))

(defn init-stream-state
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
   :current-block     nil
   :toolcall-state    {}})

(defn- finish-current-block
  [state]
  (if-let [{:keys [kind index]} (:current-block state)]
    (case kind
      :text {:state  (assoc state :current-block nil)
             :events [{:type :text-end}]}
      :thinking {:state  (assoc state :current-block nil)
                 :events [{:type :thinking-end}]}
      :tool-call (let [tool-call (get-in state [:assistant-message :content index])]
                   {:state  (assoc state :current-block nil)
                    :events [{:type      :toolcall-end
                              :id        (:id tool-call)
                              :name      (:name tool-call)
                              :arguments (:arguments tool-call)}]})
      {:state state :events []})
    {:state state :events []}))

(defn- start-text-block
  [state]
  (let [index (count (get-in state [:assistant-message :content]))]
    {:state  (-> state
                 (update-in [:assistant-message :content] conj {:type :text :text ""})
                 (assoc :current-block {:kind :text :index index}))
     :events [{:type :text-start}]}))

(defn- start-thinking-block
  [state signature]
  (let [index (count (get-in state [:assistant-message :content]))]
    {:state  (-> state
                 (update-in [:assistant-message :content] conj
                            {:type      :thinking         :thinking ""
                             :signature (or signature "")})
                 (assoc :current-block {:kind :thinking :index index}))
     :events [{:type :thinking-start}]}))

(defn- ensure-active-thinking-block
  ([state]
   (ensure-active-thinking-block state nil))
  ([state signature]
   (if (= :thinking (get-in state [:current-block :kind]))
     {:state state :events []}
     (let [{state1 :state end-events :events}   (finish-current-block state)
           {state2 :state start-events :events} (start-thinking-block state1 signature)]
       {:state state2 :events (into [] (concat end-events start-events))}))))

(defn- start-toolcall-block
  [state tool-index id name]
  (let [index (count (get-in state [:assistant-message :content]))]
    {:state  (-> state
                 (update-in [:assistant-message :content] conj {:type      :tool-call
                                                                :id        id
                                                                :name      name
                                                                :arguments {}})
                 (assoc :current-block {:kind       :tool-call
                                        :index      index
                                        :tool-index tool-index}))
     :events [{:type :toolcall-start :id id :name name}]}))

(defn- ensure-active-text-block
  [state]
  (if (= :text (get-in state [:current-block :kind]))
    {:state state :events []}
    (let [{state1 :state end-events :events}   (finish-current-block state)
          {state2 :state start-events :events} (start-text-block state1)]
      {:state state2 :events (into [] (concat end-events start-events))})))

(defn- ensure-active-toolcall-block
  [state tool-index id name]
  (if (and (= :tool-call (get-in state [:current-block :kind]))
           (= tool-index (get-in state [:current-block :tool-index])))
    {:state state :events []}
    (let [{state1 :state end-events :events}   (finish-current-block state)
          {state2 :state start-events :events} (start-toolcall-block state1 tool-index id name)]
      {:state state2 :events (into [] (concat end-events start-events))})))

(defn- non-empty-or
  [value fallback]
  (if (str/blank? (or value ""))
    fallback
    value))

(defn- extract-reasoning-delta
  [delta]
  (or (when-let [v (:reasoning_content delta)] (when (seq v) {:text v :field "reasoning_content"}))
      (when-let [v (:reasoning delta)] (when (seq v) {:text v :field "reasoning"}))
      (when-let [v (:reasoning_text delta)] (when (seq v) {:text v :field "reasoning_text"}))))

(defn- apply-reasoning-delta
  [state events reasoning-delta-map]
  (let [text                                  (:text reasoning-delta-map)
        field                                 (:field reasoning-delta-map)
        {state1 :state ensure-events :events} (ensure-active-thinking-block state field)
        index                                 (get-in state1 [:current-block :index])]
    {:state  (update-in state1 [:assistant-message :content index :thinking] str text)
     :events (into events (concat ensure-events [{:type :thinking-delta :thinking text}]))}))

(defn- apply-text-delta
  [state events text-delta]
  (let [{state1 :state ensure-events :events} (ensure-active-text-block state)
        index                                 (get-in state1 [:current-block :index])]
    {:state  (update-in state1 [:assistant-message :content index :text] str text-delta)
     :events (into events (concat ensure-events [{:type :text-delta :text text-delta}]))}))

(defn- apply-tool-call-delta
  [env state events tool-call]
  (let [tool-index                            (long (or (:index tool-call) 0))
        prior-tool                            (get-in state [:toolcall-state tool-index]
                                                      {:id "" :name "" :partial-args ""})
        id                                    (non-empty-or (:id tool-call)
                                                            (non-empty-or (:id prior-tool) ((:id/new env))))
        name                                  (non-empty-or (get-in tool-call [:function :name])
                                                            (non-empty-or (:name prior-tool) "tool"))
        {state1 :state ensure-events :events} (ensure-active-toolcall-block state tool-index id name)
        args-delta                            (or (get-in tool-call [:function :arguments]) "")
        partial-args                          (str (:partial-args prior-tool) args-delta)
        parsed-args                           (parse-json-safe env partial-args)
        index                                 (get-in state1 [:current-block :index])]
    {:state  (-> state1
                 (assoc-in [:assistant-message :content index :id] id)
                 (assoc-in [:assistant-message :content index :name] name)
                 (assoc-in [:assistant-message :content index :arguments] parsed-args)
                 (assoc-in [:toolcall-state tool-index] {:id id :name name :partial-args partial-args}))
     :events (into events
                   (concat ensure-events
                           [{:type      :toolcall-delta
                             :id        id
                             :name      name
                             :arguments parsed-args}]))}))

(defn- parse-chunk
  [env raw-chunk]
  (cond
    (map? raw-chunk) raw-chunk
    (string? raw-chunk) (parse-json-safe env raw-chunk)
    :else {}))

(defn- apply-chunk-metadata
  [state chunk]
  (let [state  (if-let [usage (:usage chunk)]
                 (assoc-in state [:assistant-message :usage] (usage->canonical usage))
                 state)
        choice (first (:choices chunk))
        state  (if-let [finish-reason (:finish_reason choice)]
                 (assoc-in state [:assistant-message :stop-reason] (map-stop-reason finish-reason))
                 state)]
    {:state state :delta (:delta choice)}))

(defn- apply-reasoning-details
  [env state delta]
  (if-let [details (:reasoning_details delta)]
    (if (sequential? details)
      (reduce
       (fn [s detail]
         (if (and (= "reasoning.encrypted" (:type detail))
                  (:id detail)
                  (:data detail))
           (let [matching-idx (->> (get-in s [:assistant-message :content])
                                   (keep-indexed (fn [i block]
                                                   (when (and (= :tool-call (:type block))
                                                              (= (:id detail) (:id block)))
                                                     i)))
                                   first)]
             (if matching-idx
               (assoc-in s [:assistant-message :content matching-idx :signature]
                         ((:json/encode env) detail))
               s))
           s))
       state
       details)
      state)
    state))

(defn- process-deltas
  [env state delta]
  (loop [state           state
         events          []
         reasoning-delta (extract-reasoning-delta delta)
         text-delta      (:content delta)
         tool-calls      (or (:tool_calls delta) [])]
    (cond
      (map? reasoning-delta)
      (let [{:keys [state events]} (apply-reasoning-delta state events reasoning-delta)]
        (recur state events nil text-delta tool-calls))

      (and (string? text-delta) (seq text-delta))
      (let [{:keys [state events]} (apply-text-delta state events text-delta)]
        (recur state events nil nil tool-calls))

      (seq tool-calls)
      (let [{:keys [state events]} (apply-tool-call-delta env state events (first tool-calls))]
        (recur state events nil nil (rest tool-calls)))

      :else
      (let [state (apply-reasoning-details env state delta)]
        {:state state :events events}))))

(>defn decode-event
       [env state raw-chunk]
       [:llx/env :llx/runtime-stream-state :llx/raw-stream-chunk => :llx/runtime-decode-event-result]
       (let [state (if (:assistant-message state)
                     state
                     (init-stream-state env (:model state)))
             chunk (parse-chunk env raw-chunk)]
         (if-not (seq chunk)
           {:state state :events []}
           (let [{:keys [state delta]} (apply-chunk-metadata state chunk)]
             (process-deltas env state delta)))))

(>defn finalize
       [env state-or-response]
       [:llx/env :llx/runtime-finalize-input => :llx/runtime-finalize-result]
       (if (contains? state-or-response :response)
         {:assistant-message (response->assistant-message env (:model state-or-response) (:response state-or-response))
          :events            []}
         (let [state-or-response             (if (:assistant-message state-or-response)
                                               state-or-response
                                               (init-stream-state env (:model state-or-response)))
               {state :state events :events} (finish-current-block state-or-response)]
           {:assistant-message (:assistant-message state)
            :events            events})))

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
                 retry-after   (errors/extract-retry-after headers)
                 provider      (name (or (:provider _model) "unknown"))]
             (throw (errors/http-status->error
                     status provider message
                     :provider-code provider-code
                     :retry-after retry-after
                     :request-id request-id
                     :body body))))
         response))

(defn- mistral-target?
  [target-model]
  (let [provider (name (or (:provider target-model) ""))
        base-url (str/lower-case (or (:base-url target-model) ""))]
    (or (= provider "mistral")
        (str/includes? base-url "mistral.ai")
        (str/includes? base-url "chutes.ai"))))

(defn- normalize-mistral-tool-id
  [id]
  (let [normalized (-> id
                       (str/replace #"[^a-zA-Z0-9]" "")
                       (subs 0 (min 9 (count (str/replace id #"[^a-zA-Z0-9]" "")))))]
    (if (< (count normalized) 9)
      (str normalized (subs "ABCDEFGHI" 0 (- 9 (count normalized))))
      normalized)))

(defn- stable-alnum-suffix
  [source-id salt]
  (let [h  (-> (hash (str source-id ":" salt))
               long
               Math/abs
               str
               str/upper-case)
        h9 (subs (str h "ABCDE") 0 5)]
    h9))

(defn- ensure-unique-mistral-id
  [used candidate source-id]
  (if-not (contains? used candidate)
    candidate
    (loop [salt 1]
      (let [prefix (subs candidate 0 4)
            alt-id (str prefix (stable-alnum-suffix source-id salt))]
        (if (contains? used alt-id)
          (recur (inc salt))
          alt-id)))))

(defn- strip-openai-responses-pipe-id
  [tool-call-id]
  (if (str/includes? tool-call-id "|")
    (first (str/split tool-call-id #"\|" 2))
    tool-call-id))

(defn- mistral-id-map
  [assistant-message]
  (let [tool-calls (->> (:content assistant-message)
                        (filter #(= :tool-call (:type %)))
                        (map :id)
                        (filter string?))]
    (:ids
     (reduce
      (fn [{:keys [used ids]} source-id]
        (let [candidate (normalize-mistral-tool-id (strip-openai-responses-pipe-id source-id))
              unique-id (ensure-unique-mistral-id used candidate source-id)]
          {:used (conj used unique-id)
           :ids  (assoc ids source-id unique-id)}))
      {:used #{} :ids {}}
      tool-calls))))

(defn normalize-tool-call-id
  [tool-call-id target-model source-assistant-message]
  (let [id (strip-openai-responses-pipe-id tool-call-id)]
    (if (mistral-target? target-model)
      (or (get (mistral-id-map source-assistant-message) tool-call-id)
          (normalize-mistral-tool-id id))
      (let [sanitized (str/replace id #"[^a-zA-Z0-9_-]" "_")]
        (if (> (count sanitized) 40)
          (subs sanitized 0 40)
          sanitized)))))

(defn adapter
  []
  {:api                    :openai-completions
   :build-request          build-request
   :open-stream            open-stream
   :decode-event           decode-event
   :finalize               finalize
   :normalize-error        normalize-error
   :supports-model?        (fn [model] (= :openai-completions (:api model)))
   :normalize-tool-call-id normalize-tool-call-id
   :transform-options      {:id-normalization-profile :openai-completions}
   :transform-context      (fn [_model context] context)})
