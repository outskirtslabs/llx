(ns llx-ai.adapters.google-generative-ai
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
  (let [cache-read (long (or (:cachedContentTokenCount usage) 0))
        prompt     (long (or (:promptTokenCount usage) 0))
        ;; Google promptTokenCount is the canonical input total.
        ;; cache-read is tracked separately but not subtracted from input.
        input      prompt
        output     (+ (long (or (:candidatesTokenCount usage) 0))
                      (long (or (:thoughtsTokenCount usage) 0)))
        total      (long (or (:totalTokenCount usage)
                             (+ input output cache-read)))
        usage*     {:input        input
                    :output       output
                    :cache-read   cache-read
                    :cache-write  0
                    :total-tokens total}]
    {:input        input
     :output       output
     :cache-read   cache-read
     :cache-write  0
     :total-tokens total
     :cost         (models/calculate-cost model usage*)}))

(defn- map-stop-reason
  [finish-reason]
  (case finish-reason
    "STOP" :stop
    "MAX_TOKENS" :length
    "BLOCKLIST" :error
    "PROHIBITED_CONTENT" :error
    "SPII" :error
    "SAFETY" :error
    "IMAGE_SAFETY" :error
    "IMAGE_PROHIBITED_CONTENT" :error
    "IMAGE_RECITATION" :error
    "IMAGE_OTHER" :error
    "RECITATION" :error
    "FINISH_REASON_UNSPECIFIED" :error
    "OTHER" :error
    "LANGUAGE" :error
    "MALFORMED_FUNCTION_CALL" :error
    "UNEXPECTED_TOOL_CALL" :error
    "NO_IMAGE" :error
    (throw (ex-info "Unknown Google finish reason" {:finish-reason finish-reason}))))

(defn- valid-thought-signature?
  [sig]
  (and (string? sig)
       (not (str/blank? sig))
       (zero? (mod (count sig) 4))
       (boolean (re-matches #"[A-Za-z0-9+/]+={0,2}" sig))))

(defn- model-supports-image?
  [model]
  (contains? (get-in model [:capabilities :input] #{}) :image))

(defn- requires-tool-call-id?
  [model-id]
  (or (str/starts-with? (or model-id "") "claude-")
      (str/starts-with? (or model-id "") "gpt-oss-")))

(defn- convert-user-content
  [model content]
  (if (string? content)
    [{:text content}]
    (let [supports-image? (model-supports-image? model)]
      (->> content
           (map (fn [block]
                  (case (:type block)
                    :text {:text (:text block)}
                    :image (when supports-image?
                             {:inlineData {:mimeType (:mime-type block)
                                           :data     (:data block)}})
                    nil)))
           (remove nil?)
           vec))))

(defn- same-model?
  [assistant-message model]
  (and (= (:provider assistant-message) (:provider model))
       (= (:api assistant-message) (:api model))
       (= (:model assistant-message) (:id model))))

(defn- gemini-3-model?
  [model]
  (str/includes? (str/lower-case (:id model)) "gemini-3"))

(defn- tool-call->historical-text
  [tool-call]
  {:text (str "[Historical context: a different model called tool \""
              (:name tool-call)
              "\" with arguments: "
              ((fnil pr-str {}) (:arguments tool-call))
              ". Do not mimic this format - use proper function calling.]")})

(defn- assistant-part
  [model assistant-message block]
  (let [same-model?       (same-model? assistant-message model)
        thought-signature (when same-model?
                            (let [sig (:signature block)]
                              (when (valid-thought-signature? sig)
                                sig)))]
    (case (:type block)
      :text
      (let [text (:text block)]
        (when (and (string? text) (not (str/blank? text)))
          (cond-> {:text text}
            thought-signature (assoc :thoughtSignature thought-signature))))

      :thinking
      (let [thinking (:thinking block)]
        (when (and (string? thinking) (not (str/blank? thinking)))
          (if same-model?
            (cond-> {:thought true :text thinking}
              thought-signature (assoc :thoughtSignature thought-signature))
            {:text thinking})))

      :tool-call
      (let [name (:name block)
            args (or (:arguments block) {})]
        (cond
          (and (gemini-3-model? model)
               (nil? thought-signature))
          (tool-call->historical-text block)

          :else
          (let [part {:functionCall (cond-> {:name name :args args}
                                      (requires-tool-call-id? (:id model)) (assoc :id (:id block)))}]
            (cond-> part
              thought-signature (assoc :thoughtSignature thought-signature)))))

      nil)))

(defn- convert-tool-result-message
  [model message]
  (let [text-content                         (->> (:content message)
                                                  (filter #(= :text (:type %)))
                                                  (map :text)
                                                  (remove nil?)
                                                  (str/join "\n"))
        image-content                        (if (model-supports-image? model)
                                               (filter #(= :image (:type %)) (:content message))
                                               [])
        has-text?                            (seq text-content)
        has-images?                          (seq image-content)
        supports-multimodal-function-result? (gemini-3-model? model)
        response-value                       (cond
                                               has-text? text-content
                                               has-images? "(see attached image)"
                                               :else "")
        image-parts                          (mapv (fn [image-block]
                                                     {:inlineData {:mimeType (:mime-type image-block)
                                                                   :data     (:data image-block)}})
                                                   image-content)
        function-response                    {:functionResponse
                                              (cond-> {:name     (:tool-name message)
                                                       :response (if (:is-error? message)
                                                                   {:error response-value}
                                                                   {:output response-value})}
                                                has-images?
                                                (cond->
                                                 (and supports-multimodal-function-result? (seq image-parts))
                                                  (assoc :parts image-parts))

                                                (requires-tool-call-id? (:id model))
                                                (assoc :id (:tool-call-id message)))}
        merged-out                           [{:role "user" :parts [function-response]}]
        merged-out                           (if (and has-images? (not supports-multimodal-function-result?))
                                               (conj merged-out
                                                     {:role  "user"
                                                      :parts (into [{:text "Tool result image:"}] image-parts)})
                                               merged-out)]
    merged-out))

(defn- convert-assistant-message
  [model message]
  (let [parts (->> (:content message)
                   (map (partial assistant-part model message))
                   (remove nil?)
                   vec)]
    (when (seq parts)
      {:role "model" :parts parts})))

(defn- convert-messages
  [model context]
  (loop [remaining (:messages context)
         out       []]
    (if-let [message (first remaining)]
      (case (:role message)
        :user
        (let [parts (convert-user-content model (:content message))]
          (recur (rest remaining)
                 (cond-> out
                   (seq parts) (conj {:role "user" :parts parts}))))

        :assistant
        (recur (rest remaining)
               (cond-> out
                 (convert-assistant-message model message)
                 (conj (convert-assistant-message model message))))

        :tool-result
        (recur (rest remaining)
               (into out (convert-tool-result-message model message)))

        (recur (rest remaining) out))
      out)))

(defn- convert-tools
  [tools]
  (when (seq tools)
    [{:functionDeclarations
      (mapv (fn [tool]
              {:name                 (:name tool)
               :description          (:description tool)
               :parametersJsonSchema (schema/malli->json-schema (or (:input-schema tool) {}))})
            tools)}]))

(defn- tool-choice->mode
  [choice]
  (case choice
    :auto "AUTO"
    :none "NONE"
    :any "ANY"
    "AUTO"))

(defn- gemini-3-pro-model?
  [model]
  (str/includes? (or (:id model) "") "3-pro"))

(defn- gemini-3-flash-model?
  [model]
  (str/includes? (or (:id model) "") "3-flash"))

(defn- gemini3-thinking-level
  [effort model]
  (if (gemini-3-pro-model? model)
    (case effort
      (:minimal :low) "LOW"
      (:medium :high) "HIGH"
      "HIGH")
    (case effort
      :minimal "MINIMAL"
      :low     "LOW"
      :medium  "MEDIUM"
      :high    "HIGH"
      "MEDIUM")))

(defn- google-thinking-budget
  [model effort]
  (let [pro-budgets   {:minimal 128 :low 2048 :medium 8192 :high 32768}
        flash-budgets {:minimal 128 :low 2048 :medium 8192 :high 24576}]
    (cond
      (str/includes? (or (:id model) "") "2.5-pro")
      (get pro-budgets effort 8192)

      (str/includes? (or (:id model) "") "2.5-flash")
      (get flash-budgets effort 8192)

      :else -1)))

(defn- reasoning->thinking-config
  [model reasoning]
  (when (and (true? (get-in model [:capabilities :reasoning?]))
             (map? reasoning))
    (let [effort (or (:level reasoning) (:effort reasoning) :medium)]
      (cond
        (or (gemini-3-pro-model? model) (gemini-3-flash-model? model))
        {:includeThoughts true
         :thinkingLevel   (gemini3-thinking-level effort model)}

        :else
        (let [budget (google-thinking-budget model effort)]
          {:includeThoughts true
           :thinkingBudget  budget})))))

(>defn build-request
       ([env model context opts]
        [:llx/env :llx/model :llx/context-map :llx/request-options => :llx/adapter-request-map]
        (build-request env model context opts false))
       ([env model context opts stream?]
        [:llx/env :llx/model :llx/context-map :llx/request-options :boolean => :llx/adapter-request-map]
        (let [api-key         (or (:api-key opts)
                                  (when-let [env-get (:env/get env)]
                                    (env-get "GEMINI_API_KEY")))
              _               (when-not (seq api-key)
                                (throw (ex-info "Google API key is required" {:provider (:provider model)})))
              thinking-config (reasoning->thinking-config model (:reasoning opts))
              config          (cond-> {}
                                (contains? opts :temperature) (assoc :temperature (:temperature opts))
                                (:max-output-tokens opts) (assoc :maxOutputTokens (:max-output-tokens opts))
                                (seq (:system-prompt context)) (assoc :systemInstruction {:role "user" :parts [{:text (:system-prompt context)}]})
                                (seq (:tools context)) (assoc :tools (convert-tools (:tools context)))
                                (:tool-choice opts) (assoc :toolConfig {:functionCallingConfig
                                                                        {:mode (tool-choice->mode (:tool-choice opts))}})
                                thinking-config (assoc :thinkingConfig thinking-config))
              payload         (cond-> {:contents (convert-messages model context)}
                                (contains? config :temperature)
                                (assoc-in [:generationConfig :temperature] (:temperature config))

                                (contains? config :maxOutputTokens)
                                (assoc-in [:generationConfig :maxOutputTokens] (:maxOutputTokens config))

                                (contains? config :thinkingConfig)
                                (assoc-in [:generationConfig :thinkingConfig] (:thinkingConfig config))

                                (contains? config :systemInstruction)
                                (assoc :systemInstruction (:systemInstruction config))

                                (contains? config :tools)
                                (assoc :tools (:tools config))

                                (contains? config :toolConfig)
                                (assoc :toolConfig (:toolConfig config)))
              base            (trim-trailing-slash (:base-url model))
              suffix          (if stream?
                                (str "/models/" (:id model) ":streamGenerateContent?alt=sse")
                                (str "/models/" (:id model) ":generateContent"))]
          (trove/log! {:level :trace
                       :id    :llx.obs/provider-payload
                       :data  {:call-id  (:call/id env)
                               :provider (:provider model)
                               :api      (:api model)
                               :model-id (:id model)
                               :stream?  stream?
                               :url      (str base suffix)
                               :payload  ((:unicode/sanitize-payload env) payload)}})
          {:method  :post
           :url     (str base suffix)
           :headers (cond-> {"Content-Type"   "application/json"
                             "x-goog-api-key" api-key}
                      (:headers model) (merge (:headers model))
                      (:headers opts) (merge (:headers opts)))
           :body    ((:json/encode env) ((:unicode/sanitize-payload env) payload))
           :as      (if stream? :stream :string)
           :throw   false})))

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
   :current-block     nil})

(defn- end-current-block
  [state]
  (if-let [{:keys [kind]} (:current-block state)]
    (case kind
      :text {:state  (assoc state :current-block nil)
             :events [{:type :text-end}]}
      :thinking {:state  (assoc state :current-block nil)
                 :events [{:type :thinking-end}]}
      {:state state :events []})
    {:state state :events []}))

(defn- append-content-block
  [state block]
  (let [idx (count (get-in state [:assistant-message :content]))]
    [(update-in state [:assistant-message :content] conj block) idx]))

(defn- ensure-active-text
  [state]
  (if (= :text (get-in state [:current-block :kind]))
    {:state state :events []}
    (let [{state1 :state end-events :events} (end-current-block state)
          [state2 idx]                       (append-content-block state1 {:type :text :text ""})]
      {:state  (assoc state2 :current-block {:kind :text :index idx})
       :events (into [] end-events)})))

(defn- ensure-active-thinking
  [state]
  (if (= :thinking (get-in state [:current-block :kind]))
    {:state state :events []}
    (let [{state1 :state end-events :events} (end-current-block state)
          [state2 idx]                       (append-content-block state1 {:type :thinking :thinking ""})]
      {:state  (assoc state2 :current-block {:kind :thinking :index idx})
       :events (into [] end-events)})))

(defn- finish-reason->state
  [state candidate]
  (if-let [finish-reason (:finishReason candidate)]
    (let [stop-reason (map-stop-reason finish-reason)
          stop-reason (if (and (= :stop stop-reason)
                               (some #(= :tool-call (:type %)) (get-in state [:assistant-message :content])))
                        :tool-use
                        stop-reason)]
      (assoc-in state [:assistant-message :stop-reason] stop-reason))
    state))

(defn- unique-tool-call-id
  [env state function-call]
  (let [provided-id (:id function-call)
        duplicate?  (some #(and (= :tool-call (:type %)) (= provided-id (:id %)))
                          (get-in state [:assistant-message :content]))]
    (if (or (not (seq (or provided-id ""))) duplicate?)
      (str (or (:name function-call) "tool") "_" ((:id/new env)))
      provided-id)))

(defn- process-part
  [env state part]
  (cond
    (contains? part :text)
    (let [text       (or (:text part) "")
          thinking?  (= true (:thought part))
          ensure-out (if thinking?
                       (ensure-active-thinking state)
                       (ensure-active-text state))
          state1     (:state ensure-out)
          idx        (get-in state1 [:current-block :index])
          state2     (if thinking?
                       (update-in state1 [:assistant-message :content idx :thinking] str text)
                       (update-in state1 [:assistant-message :content idx :text] str text))
          state3     (if (and thinking? (string? (:thoughtSignature part)) (not (str/blank? (:thoughtSignature part))))
                       (assoc-in state2 [:assistant-message :content idx :signature] (:thoughtSignature part))
                       state2)
          start-type (if thinking? :thinking-start :text-start)
          delta-type (if thinking? :thinking-delta :text-delta)]
      {:state  state3
       :events (into []
                     (concat
                      (:events ensure-out)
                      (when (not= (get-in state [:current-block :kind]) (if thinking? :thinking :text))
                        [{:type start-type}])
                      [(if thinking?
                         {:type delta-type :thinking text}
                         {:type delta-type :text text})]))})

    (:functionCall part)
    (let [{state1 :state end-events :events} (end-current-block state)
          function-call                      (:functionCall part)
          id                                 (unique-tool-call-id env state1 function-call)
          name                               (or (:name function-call) "")
          args                               (or (:args function-call) {})
          [state2 _]                         (append-content-block state1 {:type      :tool-call
                                                                           :id        id
                                                                           :name      name
                                                                           :arguments args})]
      {:state  state2
       :events (into []
                     (concat end-events
                             [{:type :toolcall-start :id id :name name}
                              {:type :toolcall-delta :id id :name name :arguments args}
                              {:type :toolcall-end :id id :name name :arguments args}]))})

    :else
    {:state state :events []}))

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
         (if-not (seq chunk)
           {:state state :events []}
           (let [candidate (first (:candidates chunk))
                 state     (if-let [usage (:usageMetadata chunk)]
                             (assoc-in state [:assistant-message :usage] (usage->canonical (:model state) usage))
                             state)]
             (loop [remaining-parts (or (get-in candidate [:content :parts]) [])
                    state           state
                    events          []]
               (if-let [part (first remaining-parts)]
                 (let [{next-state :state next-events :events} (process-part env state part)]
                   (recur (rest remaining-parts)
                          next-state
                          (into events next-events)))
                 {:state  (finish-reason->state state candidate)
                  :events events}))))))

(defn- response-content->canonical
  [env message]
  (let [parts (or (get-in message [:content :parts]) [])]
    (->> parts
         (mapcat (fn [part]
                   (cond
                     (:functionCall part)
                     [{:type      :tool-call
                       :id        (or (get-in part [:functionCall :id])
                                      (str (get-in part [:functionCall :name] "tool") "_" ((:id/new env))))
                       :name      (or (get-in part [:functionCall :name]) "")
                       :arguments (or (get-in part [:functionCall :args]) {})}]

                     (contains? part :text)
                     (let [text (:text part)]
                       (if (= true (:thought part))
                         [(cond-> {:type :thinking :thinking text}
                            (seq (:thoughtSignature part)) (assoc :signature (:thoughtSignature part)))]
                         [{:type :text :text text}]))

                     :else [])))
         vec)))

(defn- response->assistant-message
  [env model response]
  (let [body   (cond
                 (map? (:body response)) (:body response)
                 (string? (:body response)) ((:json/decode env) (:body response) {:key-fn keyword})
                 :else {})
        status (long (or (:status response) 0))]
    (when (or (< status 200) (>= status 300))
      (throw (ex-info "Google generative ai request failed"
                      {:status status
                       :error  (or (get-in body [:error :message]) body)})))
    (let [candidate   (first (:candidates body))
          content     (response-content->canonical env candidate)
          stop-reason (if candidate
                        (map-stop-reason (:finishReason candidate))
                        :stop)
          stop-reason (if (and (= :stop stop-reason)
                               (some #(= :tool-call (:type %)) content))
                        :tool-use
                        stop-reason)]
      {:role        :assistant
       :content     content
       :api         (:api model)
       :provider    (:provider model)
       :model       (:id model)
       :usage       (usage->canonical model (:usageMetadata body))
       :stop-reason stop-reason
       :timestamp   ((:clock/now-ms env))})))

(>defn finalize
       [env state-or-response]
       [:llx/env :llx/runtime-finalize-input => :llx/runtime-finalize-result]
       (if (contains? state-or-response :response)
         {:assistant-message (response->assistant-message env (:model state-or-response) (:response state-or-response))
          :events            []}
         (let [state-or-response             (if (:assistant-message state-or-response)
                                               state-or-response
                                               (init-stream-state env (:model state-or-response)))
               {state :state events :events} (end-current-block state-or-response)]
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
                 provider-code (get-in body [:error :status])
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
  {:api               :google-generative-ai
   :build-request     build-request
   :open-stream       open-stream
   :decode-event      decode-event
   :finalize          finalize
   :normalize-error   normalize-error
   :supports-model?   (fn [model] (= :google-generative-ai (:api model)))
   :transform-context (fn [_model context] context)})
