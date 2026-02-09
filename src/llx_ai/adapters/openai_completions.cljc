(ns llx-ai.adapters.openai-completions)

(defn- trim-trailing-slash
  [s]
  (if (and (string? s)
           (pos? (count s))
           (= \/ (nth s (dec (count s)))))
    (subs s 0 (dec (count s)))
    s))

(defn- parse-json-safe
  [decode-fn s]
  (try
    (decode-fn s {:key-fn keyword})
    (catch #?(:clj Exception :cljs :default) _
      {})))

(defn- map-stop-reason
  [finish-reason]
  (case finish-reason
    "stop" :stop
    "length" :length
    "tool_calls" :tool-use
    "tool" :tool-use
    "content_filter" :error
    :stop))

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

(defn- convert-image-block
  [block]
  {:type      "image_url"
   :image_url {:url (str "data:" (:mime-type block) ";base64," (:data block))}})

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
  (or (some (fn [block] (when (= :text (:type block)) (:text block))) content)
      ""))

(defn- convert-message
  [model message]
  (case (:role message)
    :user {:role    "user"
           :content (convert-user-content model (:content message))}
    :assistant (let [msg          {:role "assistant"}
                     text-content (assistant-content->string (:content message))
                     tool-calls   (->> (:content message)
                                       (filter #(= :tool-call (:type %)))
                                       (mapv (fn [tool-call]
                                               {:id       (:id tool-call)
                                                :type     "function"
                                                :function {:name      (:name tool-call)
                                                           :arguments ((fnil pr-str {}) (:arguments tool-call))}})))]
                 (cond
                   (seq tool-calls) (assoc msg :content (or text-content "") :tool_calls tool-calls)
                   (some? text-content) (assoc msg :content text-content)
                   :else nil))
    :tool-result {:role         "tool"
                  :tool_call_id (:tool-call-id message)
                  :content      (tool-result-content->string (:content message))}
    nil))

(defn- convert-messages
  [model context]
  (let [system-prompt  (:system-prompt context)
        system-message (when (seq system-prompt)
                         [{:role "system" :content system-prompt}])
        converted      (->> (:messages context)
                            (map (partial convert-message model))
                            (remove nil?))]
    (vec (concat system-message converted))))

(defn build-request
  [env model context opts]
  (let [api-key        (or (:api-key opts)
                           (when-let [env-get (:env/get env)]
                             (env-get "OPENAI_API_KEY")))
        needs-api-key? (not= :openai-compatible (:provider model))
        _              (when (and needs-api-key? (not (seq api-key)))
                         (throw (ex-info "OpenAI API key is required" {:provider (:provider model)})))
        payload        (cond-> {:model    (:id model)
                                :messages (convert-messages model context)
                                :stream   false}
                         (:max-output-tokens opts) (assoc :max_completion_tokens (:max-output-tokens opts))
                         (contains? opts :temperature) (assoc :temperature (:temperature opts))
                         (contains? opts :top-p) (assoc :top_p (:top-p opts)))
        body           ((:json/encode env) payload)
        base-url       (trim-trailing-slash (:base-url model))
        headers        (cond-> {"Content-Type" "application/json"}
                         (seq api-key) (assoc "Authorization" (str "Bearer " api-key))
                         (:headers opts) (merge (:headers opts)))]
    {:method  :post
     :url     (str base-url "/chat/completions")
     :headers headers
     :body    body
     :as      :string
     :throw   false}))

(defn- message->canonical-content
  [decode-fn message]
  (let [tool-calls   (seq (:tool_calls message))
        text-content (:content message)]
    (cond
      tool-calls
      (mapv (fn [tc]
              {:type      :tool-call
               :id        (or (:id tc) "")
               :name      (get-in tc [:function :name] "")
               :arguments (parse-json-safe decode-fn (or (get-in tc [:function :arguments]) "{}"))})
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

(defn response->assistant-message
  [env model response]
  (let [decode-fn (:json/decode env)
        body      (if (string? (:body response))
                    (decode-fn (:body response) {:key-fn keyword})
                    (:body response))
        status    (long (or (:status response) 0))]
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
       :content     (message->canonical-content decode-fn message)
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
