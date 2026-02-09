(ns llx-ai.client-test
  (:require
   [babashka.json :as json]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [llx-ai.adapters.anthropic-messages :as anthropic-messages]
   [llx-ai.adapters.openai-completions :as openai-completions]
   [llx-ai.adapters.openai-responses :as openai-responses]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.client :as sut]
   [llx-ai.client.runtime :as runtime]
   [llx-ai.registry :as registry]))

(set! *warn-on-reflection* true)

(def base-model
  {:id             "gpt-4o-mini"
   :name           "GPT-4o Mini"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(defn stub-env
  [handler]
  {:http/request          handler
   :json/encode           json/write-str
   :json/decode           (fn [s _opts] (json/read-str s {:key-fn keyword}))
   :json/decode-safe      (fn [s _opts]
                            (try
                              (json/read-str s {:key-fn keyword})
                              (catch Exception _
                                nil)))
   :http/read-body-string (fn [body] (slurp body))
   :stream/run!           runtime/run-stream!
   :registry              sut/default-registry
   :clock/now-ms          (fn [] 1730000000000)
   :id/new                (fn [] "id-1")})

(defn- sse-body
  [events]
  (let [payload (str (str/join "\n" events) "\n")]
    (java.io.ByteArrayInputStream. (.getBytes payload "UTF-8"))))

(deftest complete-openai-completions-happy-path
  (let [seen-request (atom nil)
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (json/write-str
                                           {:choices [{:finish_reason "stop"
                                                       :message       {:role    "assistant"
                                                                       :content "hello from llx"}}]
                                            :usage   {:prompt_tokens     12
                                                      :completion_tokens 8
                                                      :total_tokens      20}})}))
        context      {:messages [{:role :user :content "say hello" :timestamp 1}]}
        opts         {:api-key           "test-openai-key"
                      :max-output-tokens 128
                      :temperature       0.2}
        out          (sut/complete env base-model context opts)
        request      @seen-request
        payload      (json/read-str (:body request) {:key-fn keyword})]
    (is
     (= {:role        :assistant
         :stop-reason :stop
         :content     [{:type :text :text "hello from llx"}]
         :api         :openai-completions
         :provider    :openai
         :model       "gpt-4o-mini"
         :timestamp   1730000000000
         :usage       {:input 12 :output 8 :cache-read 0 :cache-write 0 :total-tokens 20}}
        (-> out
            (select-keys [:role :stop-reason :content :api :provider :model :timestamp])
            (assoc :usage (select-keys (:usage out) [:input :output :cache-read :cache-write :total-tokens])))))
    (is
     (= {:method  :post
         :url     "https://api.openai.com/v1/chat/completions"
         :throw   false
         :auth    "Bearer test-openai-key"
         :ctype   "application/json"
         :payload {:model                 "gpt-4o-mini"
                   :messages              [{:role "user" :content "say hello"}]
                   :stream                false
                   :max_completion_tokens 128
                   :temperature           0.2}}
        {:method  (:method request)
         :url     (:url request)
         :throw   (:throw request)
         :auth    (get-in request [:headers "Authorization"])
         :ctype   (get-in request [:headers "Content-Type"])
         :payload (select-keys payload [:model :messages :stream :max_completion_tokens :temperature])}))))

(deftest complete-openai-completions-tool-call-response
  (let [env     (stub-env
                 (fn [_request]
                   {:status 200
                    :body   (json/write-str
                             {:choices [{:finish_reason "tool_calls"
                                         :message       {:role       "assistant"
                                                         :content    nil
                                                         :tool_calls [{:id       "call_1"
                                                                       :type     "function"
                                                                       :function {:name      "search"
                                                                                  :arguments "{\"q\":\"foo\"}"}}]}}]
                              :usage   {:prompt_tokens     40
                                        :completion_tokens 3
                                        :total_tokens      43}})}))
        context {:messages [{:role :user :content "run search" :timestamp 1}]}
        out     (sut/complete env base-model context {:api-key "x"})]
    (is
     (= {:stop-reason :tool-use
         :content     [{:type :tool-call :id "call_1" :name "search" :arguments {:q "foo"}}]
         :usage       {:input 40 :output 3 :cache-read 0 :cache-write 0 :total-tokens 43}}
        {:stop-reason (:stop-reason out)
         :content     (:content out)
         :usage       (select-keys (:usage out) [:input :output :cache-read :cache-write :total-tokens])}))))

(deftest complete-openai-completions-non-2xx
  (let [env     (stub-env (fn [_request]
                            {:status 401
                             :body   (json/write-str {:error {:message "bad key"}})}))
        context {:messages [{:role :user :content "hi" :timestamp 1}]}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"OpenAI completions request failed"
         (sut/complete env base-model context {:api-key "bad"})))))

(deftest complete-openai-compatible-without-api-key
  (let [seen-request (atom nil)
        model        (assoc base-model
                            :provider :openai-compatible
                            :base-url "http://localhost:11434/v1")
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (json/write-str
                                           {:choices [{:finish_reason "stop"
                                                       :message       {:role    "assistant"
                                                                       :content "ollama ok"}}]
                                            :usage   {:prompt_tokens     3
                                                      :completion_tokens 2
                                                      :total_tokens      5}})}))
        out          (sut/complete env model {:messages [{:role :user :content "ping" :timestamp 1}]} {})
        request      @seen-request]
    (is
     (= {:role        :assistant
         :stop-reason :stop
         :content     [{:type :text :text "ollama ok"}]}
        (select-keys out [:role :stop-reason :content])))
    (is
     (= {:url  "http://localhost:11434/v1/chat/completions"
         :auth nil}
        {:url  (:url request)
         :auth (get-in request [:headers "Authorization"])}))))

(deftest stream-openai-completions-text-contract
  (let [seen-request (atom nil)
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (sse-body
                                           [(str "data: " (json/write-str {:choices [{:delta {:content "hello "}}]}))
                                            (str "data: " (json/write-str {:choices [{:delta         {:content "world"}
                                                                                      :finish_reason "stop"}]
                                                                           :usage   {:prompt_tokens     3
                                                                                     :completion_tokens 2
                                                                                     :total_tokens      5}}))
                                            "data: [DONE]"])}))
        stream       (sut/stream env base-model {:messages [{:role :user :content "say hello" :timestamp 1}]} {:api-key "x"})
        events       (event-stream/drain! stream)
        out          (event-stream/result stream)]
    (is
     (= [:start :text-start :text-delta :text-delta :text-end :done]
        (mapv :type events)))
    (is
     (= ["hello " "world"]
        (->> events
             (filter #(= :text-delta (:type %)))
             (mapv :text))))
    (is
     (= {:role        :assistant
         :stop-reason :stop
         :content     [{:type :text :text "hello world"}]
         :usage       {:input 3 :output 2 :cache-read 0 :cache-write 0 :total-tokens 5}}
        (-> out
            (select-keys [:role :stop-reason :content])
            (assoc :usage (select-keys (:usage out) [:input :output :cache-read :cache-write :total-tokens])))))
    (is
     (= true
        (get-in (json/read-str (:body @seen-request) {:key-fn keyword}) [:stream])))))

(deftest stream-openai-completions-tool-call-contract
  (let [env    (stub-env (fn [_request]
                           {:status 200
                            :body   (sse-body
                                     [(str "data: " (json/write-str {:choices [{:delta {:tool_calls [{:index    0
                                                                                                      :id       "call_1"
                                                                                                      :type     "function"
                                                                                                      :function {:name      "search"
                                                                                                                 :arguments "{\"q\":\"f"}}]}}]}))
                                      (str "data: " (json/write-str {:choices [{:delta {:tool_calls [{:index    0
                                                                                                      :function {:arguments "oo\"}"}}]}}]}))
                                      (str "data: " (json/write-str {:choices [{:delta         {}
                                                                                :finish_reason "tool_calls"}]}))
                                      "data: [DONE]"])}))
        model  (assoc base-model :id "gpt-tool")
        stream (sut/stream env model {:messages [{:role :user :content "run search" :timestamp 1}]} {:api-key "x"})
        events (event-stream/drain! stream)
        out    (event-stream/result stream)]
    (is
     (= [:start :toolcall-start :toolcall-delta :toolcall-delta :toolcall-end :done]
        (mapv :type events)))
    (is
     (= [{:id "call_1" :name "search" :arguments {}}
         {:id "call_1" :name "search" :arguments {:q "foo"}}]
        (->> events
             (filter #(= :toolcall-delta (:type %)))
             (mapv #(select-keys % [:id :name :arguments])))))
    (is
     (= {:stop-reason :tool-use
         :content     [{:type :tool-call :id "call_1" :name "search" :arguments {:q "foo"}}]}
        (select-keys out [:stop-reason :content])))))

(deftest stream-openai-completions-terminal-error-contract
  (let [env    (stub-env (fn [_request]
                           {:status 500
                            :body   (json/write-str {:error {:message "boom"}})}))
        stream (sut/stream env base-model {:messages [{:role :user :content "hi" :timestamp 1}]} {:api-key "x"})
        events (event-stream/drain! stream)
        out    (event-stream/result stream)]
    (is (= [:error] (mapv :type events)))
    (is (= :error (:stop-reason out)))
    (is (string? (:error-message out)))
    (is (.contains ^String (:error-message out) "boom"))))

(deftest opts-registry-overrides-env-registry
  (let [seen-request (atom nil)
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (json/write-str
                                           {:choices [{:finish_reason "stop"
                                                       :message       {:role    "assistant"
                                                                       :content "override ok"}}]
                                            :usage   {:prompt_tokens     1
                                                      :completion_tokens 1
                                                      :total_tokens      2}})}))
        env          (assoc env :registry (registry/immutable-registry))
        out          (sut/complete env base-model {:messages [{:role :user :content "ping" :timestamp 1}]}
                                   {:api-key  "x"
                                    :registry sut/default-registry})]
    (is (= "override ok" (get-in out [:content 0 :text])))
    (is (= "gpt-4o-mini" (get-in (json/read-str (:body @seen-request) {:key-fn keyword}) [:model])))))

(deftest complete-transforms-context-before-build-request
  (let [captured-context (atom nil)
        adapter          {:api                    :anthropic-messages
                          :build-request          (fn [_env _model context _opts _stream?]
                                                    (reset! captured-context context)
                                                    {:method :post :url "https://example.invalid" :headers {} :body "{}"})
                          :open-stream            (fn [_env _model _request] nil)
                          :decode-event           (fn [_env _state _chunk] {:state {} :events []})
                          :finalize               (fn [_env {:keys [model]}]
                                                    {:assistant-message {:role        :assistant
                                                                         :content     [{:type :text :text "ok"}]
                                                                         :api         (:api model)
                                                                         :provider    (:provider model)
                                                                         :model       (:id model)
                                                                         :usage       {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                                                                                       :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                                                                         :stop-reason :stop
                                                                         :timestamp   1730000000000}
                                                     :events            []})
                          :normalize-error        (fn [_env _ex _state] nil)
                          :supports-model?        (fn [_model] true)
                          :normalize-tool-call-id (fn [_id _target _source] "norm_call_1")}
        registry         (registry/register-adapter (registry/immutable-registry) adapter "test")
        env              (-> (stub-env (fn [_request] {:status 200 :body "{}"}))
                             (assoc :registry registry)
                             (assoc :clock/now-ms (fn [] 4242)))
        model            (assoc base-model
                                :id "claude-sonnet-4-5"
                                :provider :anthropic
                                :api :anthropic-messages)
        context          {:messages [{:role :user :content "use tool" :timestamp 1}
                                     {:role        :assistant
                                      :content     [{:type :tool-call :id "call_orig" :name "echo" :arguments {:message "hello"}}]
                                      :api         :openai-responses
                                      :provider    :openai
                                      :model       "gpt-5-mini"
                                      :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                                    :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                                      :stop-reason :tool-use
                                      :timestamp   2}
                                     {:role :user :content "continue" :timestamp 3}]}]
    (sut/complete env model context {})
    (is (= [:user :assistant :tool-result :user]
           (mapv :role (:messages @captured-context))))
    (is (= "norm_call_1" (get-in @captured-context [:messages 1 :content 0 :id])))
    (is (= "norm_call_1" (get-in @captured-context [:messages 2 :tool-call-id])))
    (is (= true (get-in @captured-context [:messages 2 :is-error?])))
    (is (= 4242 (get-in @captured-context [:messages 2 :timestamp])))))

(deftest complete-openai-completions-mistral-normalizes-pipe-tool-id
  (let [pipe-id      "call_pAYbIr76hXIjncD9UE4eGfnS|t5nnb2qYMFWGSsr13fhCd1CaCu3t3qONEPuOudu4HSVEtA8YJSL6FAZUxvoOoD792VIJWl91g87EdqsCWp9krVsdBysQoDaf9lMCLb8BS4EYi4gQd5kBQBYLlgD71PYwvf+TbMD9J9/5OMD42oxSRj8H+vRf78/l2Xla33LWz4nOgsddBlbvabICRs8GHt5C9PK5keFtzyi3lsyVKNlfduK3iphsZqs4MLv4zyGJnvZo/+QzShyk5xnMSQX/f98+aEoNflEApCdEOXipipgeiNWnpFSHbcwmMkZoJhURNu+JEz3xCh1mrXeYoN5o+trLL3IXJacSsLYXDrYTipZZbJFRPAucgbnjYBC+/ZzJOfkwCs+Gkw7EoZR7ZQgJ8ma+9586n4tT4cI8DEhBSZsWMjrCt8dxKg=="
        seen-request (atom nil)
        model        {:id             "devstral-medium-latest"
                      :name           "Devstral Medium"
                      :provider       :mistral
                      :api            :openai-completions
                      :base-url       "https://api.mistral.ai/v1"
                      :context-window 128000
                      :max-tokens     8192
                      :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                      :capabilities   {:reasoning? false :input #{:text}}}
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (json/write-str
                                           {:choices [{:finish_reason "stop"
                                                       :message       {:role    "assistant"
                                                                       :content "ok"}}]
                                            :usage   {:prompt_tokens     1
                                                      :completion_tokens 1
                                                      :total_tokens      2}})}))
        context      {:messages [{:role :user :content "use echo" :timestamp 1}
                                 {:role        :assistant
                                  :content     [{:type :tool-call :id pipe-id :name "echo" :arguments {:message "hello"}}]
                                  :api         :openai-responses
                                  :provider    :openai
                                  :model       "gpt-5-mini"
                                  :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                                :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                                  :stop-reason :tool-use
                                  :timestamp   2}
                                 {:role         :tool-result
                                  :tool-call-id pipe-id
                                  :tool-name    "echo"
                                  :content      [{:type :text :text "hello"}]
                                  :is-error?    false
                                  :timestamp    3}
                                 {:role :user :content "say hi" :timestamp 4}]}
        _            (sut/complete env model context {:api-key "x"})
        payload      (json/read-str (:body @seen-request) {:key-fn keyword})
        call-id      (get-in payload [:messages 1 :tool_calls 0 :id])
        result-id    (get-in payload [:messages 2 :tool_call_id])]
    (is (= call-id result-id))
    (is (= (openai-completions/normalize-tool-call-id
            pipe-id
            model
            (second (:messages context)))
           call-id))
    (is (re-matches #"[A-Za-z0-9]{9}" call-id))
    (is (not (str/includes? call-id "|")))))

(deftest complete-anthropic-messages-dispatches-through-registry
  (let [seen-request (atom nil)
        model        {:id             "claude-sonnet-4-5"
                      :name           "Claude Sonnet 4.5"
                      :provider       :anthropic
                      :api            :anthropic-messages
                      :base-url       "https://api.anthropic.com"
                      :context-window 200000
                      :max-tokens     8192
                      :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                      :capabilities   {:reasoning? true :input #{:text}}}
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (json/write-str
                                           {:content     [{:type "text" :text "hello from claude"}]
                                            :stop_reason "end_turn"
                                            :usage       {:input_tokens 4 :output_tokens 3}})}))
        out          (sut/complete env model {:messages [{:role :user :content "say hi" :timestamp 1}]}
                                   {:api-key "anthropic-test-key"})
        payload      (json/read-str (:body @seen-request) {:key-fn keyword})]
    (is (= "https://api.anthropic.com/v1/messages" (:url @seen-request)))
    (is (= "anthropic-test-key" (get-in @seen-request [:headers "x-api-key"])))
    (is (= "claude-sonnet-4-5" (:model payload)))
    (is (= [{:role "user" :content "say hi"}] (:messages payload)))
    (is (= :assistant (:role out)))
    (is (= :stop (:stop-reason out)))))

(deftest complete-google-generative-ai-dispatches-through-registry
  (let [seen-request (atom nil)
        model        {:id             "gemini-2.5-flash"
                      :name           "Gemini 2.5 Flash"
                      :provider       :google
                      :api            :google-generative-ai
                      :base-url       "https://generativelanguage.googleapis.com/v1beta"
                      :context-window 1048576
                      :max-tokens     8192
                      :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                      :capabilities   {:reasoning? true :input #{:text}}}
        env          (-> (stub-env (fn [request]
                                     (reset! seen-request request)
                                     {:status 200
                                      :body   (json/write-str
                                               {:candidates    [{:content      {:parts [{:text "hello from gemini"}]}
                                                                 :finishReason "STOP"}]
                                                :usageMetadata {:promptTokenCount     4
                                                                :candidatesTokenCount 3
                                                                :totalTokenCount      7}})}))
                         (assoc :env/get (fn [k]
                                           (case k
                                             "GEMINI_API_KEY" "google-key"
                                             nil))))
        out          (sut/complete env model {:messages [{:role :user :content "say hi" :timestamp 1}]} {})
        payload      (json/read-str (:body @seen-request) {:key-fn keyword})]
    (is (= "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
           (:url @seen-request)))
    (is (= "google-key" (get-in @seen-request [:headers "x-goog-api-key"])))
    (is (= "say hi" (get-in payload [:contents 0 :parts 0 :text])))
    (is (= :assistant (:role out)))
    (is (= :stop (:stop-reason out)))))

(deftest stream-anthropic-messages-dispatches-through-registry
  (let [model  {:id             "claude-sonnet-4-5"
                :name           "Claude Sonnet 4.5"
                :provider       :anthropic
                :api            :anthropic-messages
                :base-url       "https://api.anthropic.com"
                :context-window 200000
                :max-tokens     8192
                :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                :capabilities   {:reasoning? true :input #{:text}}}
        env    (stub-env (fn [_request]
                           {:status 200
                            :body   (sse-body
                                     [(str "data: " (json/write-str {:type    "message_start"
                                                                     :message {:usage {:input_tokens 1 :output_tokens 0}}}))
                                      (str "data: " (json/write-str {:type          "content_block_start"
                                                                     :index         0
                                                                     :content_block {:type "text"}}))
                                      (str "data: " (json/write-str {:type  "content_block_delta"
                                                                     :index 0
                                                                     :delta {:type "text_delta" :text "hello"}}))
                                      (str "data: " (json/write-str {:type "content_block_stop" :index 0}))
                                      (str "data: " (json/write-str {:type  "message_delta"
                                                                     :delta {:stop_reason "end_turn"}
                                                                     :usage {:input_tokens 1 :output_tokens 1}}))
                                      "data: [DONE]"])}))
        stream (sut/stream env model {:messages [{:role :user :content "say hi" :timestamp 1}]}
                           {:api-key "anthropic-test-key"})
        events (event-stream/drain! stream)
        out    (event-stream/result stream)]
    (is (= [:start :text-start :text-delta :text-end :done]
           (mapv :type events)))
    (is (= "hello" (get-in out [:content 0 :text])))
    (is (= :stop (:stop-reason out)))))

(deftest stream-google-generative-ai-dispatches-through-registry
  (let [model  {:id             "gemini-2.5-flash"
                :name           "Gemini 2.5 Flash"
                :provider       :google
                :api            :google-generative-ai
                :base-url       "https://generativelanguage.googleapis.com/v1beta"
                :context-window 1048576
                :max-tokens     8192
                :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                :capabilities   {:reasoning? true :input #{:text}}}
        env    (-> (stub-env (fn [_request]
                               {:status 200
                                :body   (sse-body
                                         [(str "data: " (json/write-str {:candidates [{:content {:parts [{:text "hello "}]}}]}))
                                          (str "data: " (json/write-str {:candidates    [{:content      {:parts [{:text "world"}]}
                                                                                          :finishReason "STOP"}]
                                                                         :usageMetadata {:promptTokenCount     3
                                                                                         :candidatesTokenCount 2
                                                                                         :totalTokenCount      5}}))
                                          "data: [DONE]"])}))
                   (assoc :env/get (fn [k]
                                     (case k
                                       "GEMINI_API_KEY" "google-key"
                                       nil))))
        stream (sut/stream env model {:messages [{:role :user :content "say hi" :timestamp 1}]} {})
        events (event-stream/drain! stream)
        out    (event-stream/result stream)]
    (is (= [:start :text-start :text-delta :text-delta :text-end :done]
           (mapv :type events)))
    (is (= "hello world" (get-in out [:content 0 :text])))
    (is (= :stop (:stop-reason out)))))

(deftest anthropic-normalization-hook-sanitizes-tool-call-id
  (let [normalized (anthropic-messages/normalize-tool-call-id
                    "call|tool/result?bad"
                    {:provider :anthropic :api :anthropic-messages :id "claude-sonnet-4-5"}
                    nil)]
    (is (re-matches #"[A-Za-z0-9_-]+" normalized))
    (is (<= (count normalized) 64))))

(deftest complete-openai-responses-dispatches-through-registry
  (let [seen-request (atom nil)
        model        {:id             "gpt-5-mini"
                      :name           "GPT-5 Mini"
                      :provider       :openai
                      :api            :openai-responses
                      :base-url       "https://api.openai.com/v1"
                      :context-window 400000
                      :max-tokens     128000
                      :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                      :capabilities   {:reasoning? true :input #{:text}}}
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (json/write-str
                                           {:status "completed"
                                            :output [{:type    "message"
                                                      :id      "msg_1"
                                                      :role    "assistant"
                                                      :content [{:type "output_text"
                                                                 :text "hello from responses"}]}]
                                            :usage  {:input_tokens 6 :output_tokens 4 :total_tokens 10}})}))
        out          (sut/complete env model {:messages [{:role :user :content "say hi" :timestamp 1}]}
                                   {:api-key "openai-test-key"})
        payload      (json/read-str (:body @seen-request) {:key-fn keyword})]
    (is (= "https://api.openai.com/v1/responses" (:url @seen-request)))
    (is (= "Bearer openai-test-key" (get-in @seen-request [:headers "Authorization"])))
    (is (= "gpt-5-mini" (:model payload)))
    (is (= false (:stream payload)))
    (is (= :assistant (:role out)))
    (is (= :stop (:stop-reason out)))
    (is (= "hello from responses" (get-in out [:content 0 :text])))))

(deftest stream-openai-responses-dispatches-through-registry
  (let [model  {:id             "gpt-5-mini"
                :name           "GPT-5 Mini"
                :provider       :openai
                :api            :openai-responses
                :base-url       "https://api.openai.com/v1"
                :context-window 400000
                :max-tokens     128000
                :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                :capabilities   {:reasoning? true :input #{:text}}}
        env    (stub-env (fn [_request]
                           {:status 200
                            :body   (sse-body
                                     [(str "data: " (json/write-str {:type "response.output_item.added"
                                                                     :item {:type    "message"
                                                                            :id      "msg_1"
                                                                            :content []}}))
                                      (str "data: " (json/write-str {:type "response.content_part.added"
                                                                     :part {:type "output_text" :text ""}}))
                                      (str "data: " (json/write-str {:type  "response.output_text.delta"
                                                                     :delta "hello"}))
                                      (str "data: " (json/write-str {:type "response.output_item.done"
                                                                     :item {:type    "message"
                                                                            :id      "msg_1"
                                                                            :content [{:type "output_text" :text "hello"}]}}))
                                      (str "data: " (json/write-str {:type     "response.completed"
                                                                     :response {:status "completed"
                                                                                :usage  {:input_tokens 2 :output_tokens 1 :total_tokens 3}}}))
                                      "data: [DONE]"])}))
        stream (sut/stream env model {:messages [{:role :user :content "say hi" :timestamp 1}]}
                           {:api-key "openai-test-key"})
        events (event-stream/drain! stream)
        out    (event-stream/result stream)]
    (is (= [:start :text-start :text-delta :text-end :done]
           (mapv :type events)))
    (is (= "hello" (get-in out [:content 0 :text])))
    (is (= :stop (:stop-reason out)))))

(deftest openai-responses-normalization-hook-normalizes-pipe-id
  (let [normalized (openai-responses/normalize-tool-call-id
                    "call_bad|item/with+chars=="
                    {:provider :openai :api :openai-responses :id "gpt-5-mini"}
                    nil)]
    (is (string? normalized))
    (is (str/includes? normalized "|"))))

(deftest complete-rejects-context-map-with-unknown-keys
  (let [env     (stub-env (fn [_request]
                            {:status 200
                             :body   (json/write-str
                                      {:choices [{:finish_reason "stop"
                                                  :message       {:role    "assistant"
                                                                  :content "ok"}}]
                                       :usage   {:prompt_tokens 1 :completion_tokens 1 :total_tokens 2}})}))
        context {:messages [{:role :user :content "hello" :timestamp 1}]
                 :unknown  true}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Schema validation failed"
         (sut/complete env base-model context {:api-key "x"})))))

(deftest complete-rejects-non-map-opts-with-schema-error
  (let [env     (stub-env (fn [_request]
                            {:status 200
                             :body   (json/write-str
                                      {:choices [{:finish_reason "stop"
                                                  :message       {:role    "assistant"
                                                                  :content "ok"}}]
                                       :usage   {:prompt_tokens 1 :completion_tokens 1 :total_tokens 2}})}))
        context {:messages [{:role :user :content "hello" :timestamp 1}]}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Schema validation failed"
         (sut/complete env base-model context [1 2])))))

(deftest complete-fails-fast-on-unknown-openai-completions-stop-reason
  (let [env     (stub-env (fn [_request]
                            {:status 200
                             :body   (json/write-str
                                      {:choices [{:finish_reason "unknown_new_reason"
                                                  :message       {:role    "assistant"
                                                                  :content "ok"}}]
                                       :usage   {:prompt_tokens 2 :completion_tokens 1 :total_tokens 3}})}))
        context {:messages [{:role :user :content "hello" :timestamp 1}]}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown OpenAI completions stop reason"
         (sut/complete env base-model context {:api-key "x"})))))

(deftest complete-rejects-malformed-adapter-finalize-result
  (let [adapter {:api             :openai-completions
                 :build-request   (fn [_env _model _context _opts _stream?]
                                    {:method :post :url "https://example.invalid" :headers {} :body "{}"})
                 :open-stream     (fn [_env _model _request] nil)
                 :decode-event    (fn [_env state _chunk] {:state state :events []})
                 :finalize        (fn [_env _state] {:assistant-message {:role :assistant}})
                 :normalize-error (fn [_env _ex _partial] {})
                 :supports-model? (fn [_model] true)}
        reg     (registry/register-adapter (registry/immutable-registry) adapter "test")
        env     (-> (stub-env (fn [_request] {:status 200 :body "{}"}))
                    (assoc :registry reg))
        context {:messages [{:role :user :content "hello" :timestamp 1}]}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Schema validation failed"
         (sut/complete env base-model context {:api-key "x"})))))
