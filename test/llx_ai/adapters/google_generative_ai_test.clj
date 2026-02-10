(ns llx-ai.adapters.google-generative-ai-test
  (:require
   [babashka.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [llx-ai.test-util :as util]
   [llx-ai.adapters.google-generative-ai :as sut]
   [llx-ai.live.models :as live-models]
   [llx-ai.utils.unicode :as unicode]))

(set! *warn-on-reflection* true)

(def google-model
  {:id             "gemini-2.5-flash"
   :name           "Gemini 2.5 Flash"
   :provider       :google
   :api            :google-generative-ai
   :base-url       "https://generativelanguage.googleapis.com/v1beta"
   :context-window 1048576
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(def priced-google-model
  (assoc google-model :cost {:input 1000.0 :output 2000.0 :cache-read 3000.0 :cache-write 4000.0}))

(def google-text-only-model
  (assoc-in google-model [:capabilities :input] #{:text}))

(def google-gemini3-model
  (assoc google-model :id "gemini-3-flash-preview"))

(defn- fixture
  [name]
  (-> (str "test/llx_ai/fixtures/google/" name ".edn")
      slurp
      edn/read-string))

(defn- stub-env
  ([]
   (stub-env {}))
  ([overrides]
   (merge
    {:http/request             (fn [_] {:status 200 :body "{}"})
     :json/encode              json/write-str
     :json/decode              (fn [s _opts] (json/read-str s {:key-fn keyword}))
     :json/decode-safe         (fn [s _opts]
                                 (try
                                   (json/read-str s {:key-fn keyword})
                                   (catch Exception _ nil)))
     :clock/now-ms             (fn [] 1730000000000)
     :id/new                   (fn [] "generated-id")
     :env/get                  (fn [k]
                                 (case k
                                   "GEMINI_API_KEY" "google-key"
                                   nil))
     :unicode/sanitize-payload unicode/sanitize-payload}
    overrides)))

(deftest build-request-converts-canonical-context-to-google-payload
  (let [context (fixture "request_context")
        request (sut/build-request (stub-env) google-model context {:max-output-tokens 256 :temperature 0.25} false)
        payload (json/read-str (:body request) {:key-fn keyword})]
    (is (= {:method  :post
            :url     "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
            :headers {"Content-Type"   "application/json"
                      "x-goog-api-key" "google-key"}
            :as      :string
            :throw   false}
           (dissoc request :body)))
    (is (= {:contents
            [{:role  "user"
              :parts [{:text "Please use the tool for value 21."}
                      {:inlineData {:mimeType "image/png" :data "aGVsbG8="}}]}
             {:role  "model"
              :parts [{:thought true :text "Need tool." :thoughtSignature "c2lnX3RoaW5raW5n"}
                      {:text "Calling tool."}
                      {:functionCall     {:name "double_number" :args {:value 21}}
                       :thoughtSignature "c2lnX3Rvb2w="}]}
             {:role  "user"
              :parts [{:functionResponse {:name "double_number" :response {:output "42"}}}]}
             {:role  "user"
              :parts [{:text "Tool result image:"}
                      {:inlineData {:mimeType "image/png" :data "aGVsbG8="}}]}
             {:role "user" :parts [{:text "Now summarize in one sentence."}]}]
            :generationConfig
            {:temperature 0.25 :maxOutputTokens 256}
            :systemInstruction
            {:role "user" :parts [{:text "You are a helpful assistant."}]}
            :tools
            [{:functionDeclarations
              [{:name                 "double_number"
                :description          "Double an integer value."
                :parametersJsonSchema {:type       "object"
                                       :properties {:value {:type "integer"}}
                                       :required   ["value"]}}]}]}
           payload))))

(deftest adapter-registers-tool-call-id-normalizer-with-model-id-gate
  (let [normalize-id (:normalize-tool-call-id (sut/adapter))
        gated-model  (assoc google-model :id "claude-sonnet-4-5")
        raw-id       "call/with+symbols=="
        long-id      (apply str (repeat 80 "a"))]
    (is (fn? normalize-id))
    (is (= "call_with_symbols__"
           (normalize-id raw-id gated-model nil)))
    (is (= 64
           (count (normalize-id long-id gated-model nil))))
    (is (= raw-id
           (normalize-id raw-id google-model nil)))))

(deftest google-normalizer-matches-upstream-issue-1022-test-id-under-gate
  (let [normalize-id (:normalize-tool-call-id (sut/adapter))
        gated-model  (assoc google-model :id "claude-sonnet-4-5")
        out-id       (normalize-id live-models/upstream-failing-tool-call-id gated-model nil)]
    (is (= 64 (count out-id)))
    (is (re-matches #"[A-Za-z0-9_-]{64}" out-id))))

(deftest build-request-emits-provider-payload-trove-signal
  (util/with-captured-logs [logs*]
    (let [context {:messages [{:role :user :content "hello" :timestamp 1}]}
          request (sut/build-request (stub-env) google-model context {} false)
          payload (json/read-str (:body request) {:key-fn keyword})
          event   (util/first-event logs* :llx.obs/provider-payload)]
      (is (util/submap?
           {:id    :llx.obs/provider-payload
            :level :trace
            :data  {:provider :google
                    :api      :google-generative-ai
                    :model-id "gemini-2.5-flash"
                    :stream?  false
                    :url      "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
                    :payload  payload}}
           (util/strip-generated event [:call-id]))))))

(deftest build-request-tool-result-image-forwarding-by-model-capability
  (let [context            {:messages [{:role      :user
                                        :content   "use the tool"
                                        :timestamp 1}
                                       {:role         :tool-result
                                        :tool-call-id "call_1"
                                        :tool-name    "vision"
                                        :content      [{:type :image :data "aGVsbG8=" :mime-type "image/png"}]
                                        :is-error?    false
                                        :timestamp    2}]}
        multimodal-payload (-> (sut/build-request (stub-env) google-gemini3-model context {} false)
                               :body
                               (json/read-str {:key-fn keyword}))
        text-only-payload  (-> (sut/build-request (stub-env) google-text-only-model context {} false)
                               :body
                               (json/read-str {:key-fn keyword}))]
    (testing "gemini-3 multimodal model forwards image in functionResponse parts"
      (is (= {:contents
              [{:role "user" :parts [{:text "use the tool"}]}
               {:role  "user"
                :parts [{:functionResponse
                         {:name     "vision"
                          :response {:output "(see attached image)"}
                          :parts    [{:inlineData {:mimeType "image/png" :data "aGVsbG8="}}]}}]}]}
             multimodal-payload)))
    (testing "text-only model drops images from functionResponse"
      (is (= {:contents
              [{:role "user" :parts [{:text "use the tool"}]}
               {:role  "user"
                :parts [{:functionResponse {:name "vision" :response {:output ""}}}]}]}
             text-only-payload)))))

(deftest decode-event-stream-contract
  (let [env       (stub-env)
        chunks    (fixture "stream_events")
        reduced   (reduce (fn [{:keys [state events]} chunk]
                            (let [{next-state :state next-events :events}
                                  (sut/decode-event env state (json/write-str chunk))]
                              {:state  next-state
                               :events (into events next-events)}))
                          {:state  {:model google-model}
                           :events []}
                          chunks)
        finalized (sut/finalize env (:state reduced))]
    (is (= [{:type :thinking-start}
            {:type :thinking-delta :thinking "thinking one"}
            {:type :thinking-end}
            {:type :text-start}
            {:type :text-delta :text " text one"}
            {:type :text-end}
            {:type :toolcall-start :id "call_stream" :name "double_number"}
            {:type :toolcall-delta :id "call_stream" :name "double_number" :arguments {:value 21}}
            {:type :toolcall-end :id "call_stream" :name "double_number" :arguments {:value 21}}]
           (:events reduced)))
    (is (= {:role        :assistant
            :content     [{:type :thinking :thinking "thinking one" :signature "sig-a"}
                          {:type :text :text " text one"}
                          {:type :tool-call :id "call_stream" :name "double_number" :arguments {:value 21}}]
            :api         :google-generative-ai
            :provider    :google
            :model       "gemini-2.5-flash"
            :usage       {:input 12                                                                   :output 5 :cache-read 2 :cache-write 0 :total-tokens 19
                          :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
            :stop-reason :tool-use
            :timestamp   1730000000000}
           (:assistant-message finalized)))))

(deftest finalize-calculates-usage-costs
  (let [response {:status 200
                  :body   (json/write-str
                           {:candidates    [{:content      {:parts [{:text "ok"}]}
                                             :finishReason "STOP"}]
                            :usageMetadata {:promptTokenCount        100
                                            :candidatesTokenCount    50
                                            :thoughtsTokenCount      25
                                            :cachedContentTokenCount 10
                                            :totalTokenCount         185}})}
        out      (sut/finalize (stub-env) {:model priced-google-model :response response})]
    (is (= {:input 0.1 :output 0.15 :cache-read 0.03 :cache-write 0.0 :total 0.28}
           (get-in out [:assistant-message :usage :cost])))))

(deftest decode-event-stream-usage-calculates-costs
  (let [state {:model priced-google-model}
        chunk {:candidates    [{:content {:parts [{:text "ok"}]}}]
               :usageMetadata {:promptTokenCount        100
                               :candidatesTokenCount    50
                               :thoughtsTokenCount      25
                               :cachedContentTokenCount 10
                               :totalTokenCount         185}}
        out   (sut/decode-event (stub-env) state (json/write-str chunk))]
    (is (= {:input 0.1 :output 0.15 :cache-read 0.03 :cache-write 0.0 :total 0.28}
           (get-in out [:state :assistant-message :usage :cost])))))

(deftest thought-signature-alone-is-not-thinking
  (let [env    (stub-env)
        state  {:model google-model}
        chunk  {:candidates [{:content {:parts [{:text "plain text" :thoughtSignature "opaque"}]}}]}
        result (sut/decode-event env state (json/write-str chunk))]
    (is (= [{:type :text-start}
            {:type :text-delta :text "plain text"}]
           (:events result)))
    (is (= {:kind :text :index 0}
           (get-in result [:state :current-block])))
    (is (= [{:type :text :text "plain text"}]
           (get-in result [:state :assistant-message :content])))))

(deftest decode-event-tool-call-missing-args-defaults-to-empty-map
  (let [env    (stub-env)
        chunk  {:candidates [{:content      {:parts [{:functionCall {:name "get_status"}}]}
                              :finishReason "STOP"}]}
        result (sut/decode-event env {:model google-model} (json/write-str chunk))
        done   (sut/finalize env (:state result))]
    (is (= [{:type :toolcall-start :id "get_status_generated-id" :name "get_status"}
            {:type :toolcall-delta :id "get_status_generated-id" :name "get_status" :arguments {}}
            {:type :toolcall-end :id "get_status_generated-id" :name "get_status" :arguments {}}]
           (:events result)))
    (is (= [{:type :tool-call :id "get_status_generated-id" :name "get_status" :arguments {}}]
           (get-in done [:assistant-message :content])))
    (is (= :tool-use (get-in done [:assistant-message :stop-reason])))))

(deftest finalize-throws-on-unknown-stop-reason
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unknown Google finish reason"
       (sut/finalize
        (stub-env)
        {:model    google-model
         :response {:status 200
                    :body   (json/write-str
                             {:candidates    [{:content      {:parts [{:text "ok"}]}
                                               :finishReason "BRAND_NEW"}]
                              :usageMetadata {:promptTokenCount     1
                                              :candidatesTokenCount 1
                                              :totalTokenCount      2}})}}))))

(deftest open-stream-non-2xx-throws-structured-error
  (let [ex (try
             (sut/open-stream
              (stub-env {:http/request (fn [_]
                                         {:status 401
                                          :body   (json/write-str {:error {:message "bad key"}})})})
              google-model
              {:method :post :url "https://example.invalid"})
             (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:type         :llx/authentication-error
            :message      "bad key"
            :recoverable? false
            :provider     "google"
            :http-status  401}
           (ex-data ex)))))

(deftest stream-error-normalization-contract
  (let [out (sut/normalize-error
             (stub-env)
             (ex-info "stream failed" {:status 500 :error "bad gateway"})
             {:model             google-model
              :assistant-message {:role        :assistant
                                  :content     [{:type :text :text "partial"}]
                                  :api         :google-generative-ai
                                  :provider    :google
                                  :model       "gemini-2.5-flash"
                                  :usage       {:input        0
                                                :output       0
                                                :cache-read   0
                                                :cache-write  0
                                                :total-tokens 0
                                                :cost         {:input       0.0
                                                               :output      0.0
                                                               :cache-read  0.0
                                                               :cache-write 0.0
                                                               :total       0.0}}
                                  :stop-reason :stop
                                  :timestamp   1730000000000}})]
    (is (= {:role          :assistant
            :content       [{:type :text :text "partial"}]
            :api           :google-generative-ai
            :provider      :google
            :model         "gemini-2.5-flash"
            :usage         {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                            :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
            :stop-reason   :error
            :error-message "stream failed: bad gateway"
            :timestamp     1730000000000}
           out))))

(def google-gemini3-pro-model
  (assoc google-model :id "gemini-3-pro-preview"))

(def google-25-pro-model
  (assoc google-model :id "gemini-2.5-pro-preview"))

(defn- thinking-config
  [model reasoning-level]
  (let [req     (sut/build-request (stub-env) model
                                   {:messages [{:role :user :content "think" :timestamp 1}]}
                                   (if reasoning-level {:reasoning {:level reasoning-level}} {})
                                   false)
        payload (json/read-str (:body req) {:key-fn keyword})]
    (get-in payload [:generationConfig :thinkingConfig])))

(deftest reasoning-config-gemini-3-pro-uses-level
  (is (= {:includeThoughts true :thinkingLevel "HIGH"}
         (thinking-config google-gemini3-pro-model :medium))))

(deftest reasoning-config-gemini-3-flash-uses-level
  (is (= {:includeThoughts true :thinkingLevel "LOW"}
         (thinking-config google-gemini3-model :low))))

(deftest reasoning-config-gemini-3-pro-clamps-low-effort
  (is (= {:includeThoughts true :thinkingLevel "LOW"}
         (thinking-config google-gemini3-pro-model :low))))

(deftest reasoning-config-gemini-25-pro-budget-differs-from-flash
  (is (= {:includeThoughts true :thinkingBudget 32768}
         (thinking-config google-25-pro-model :high)))
  (is (= {:includeThoughts true :thinkingBudget 24576}
         (thinking-config google-model :high))))

(deftest reasoning-config-omitted-when-model-not-reasoning
  (is (nil? (thinking-config (assoc-in google-model [:capabilities :reasoning?] false)
                             :high))))

(deftest map-stop-reason-fail-fast-on-unknown
  (testing "unknown finish reason in stream decode fails fast"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown Google finish reason"
         (sut/decode-event
          (stub-env)
          {:model google-model}
          (json/write-str {:candidates [{:finishReason "UNRECOGNIZED_REASON"}]}))))))

(defn- string-with-unpaired-high
  []
  (str "Hello " (String. (char-array [(char 0xD83D)])) " World"))

(defn- valid-emoji-string
  []
  (str "Hello " (String. (char-array [(char 0xD83D) (char 0xDE48)])) " World"))

(deftest build-request-sanitizes-unpaired-surrogates-in-user-and-system
  (let [env      (stub-env)
        context  {:system-prompt (string-with-unpaired-high)
                  :messages      [{:role :user :content (string-with-unpaired-high) :timestamp 1}]}
        request  (sut/build-request env google-model context {} false)
        payload  (json/read-str (:body request) {:key-fn keyword})
        sys-text (get-in payload [:systemInstruction :parts 0 :text])
        usr-text (get-in payload [:contents 0 :parts 0 :text])]
    (is (not (str/includes? (str sys-text) (String. (char-array [(char 0xD83D)]))))
        "system prompt should not contain unpaired high surrogate")
    (is (not (str/includes? (str usr-text) (String. (char-array [(char 0xD83D)]))))
        "user text should not contain unpaired high surrogate")))

(deftest build-request-preserves-valid-emoji-surrogate-pairs
  (let [env      (stub-env)
        emoji    (valid-emoji-string)
        context  {:messages [{:role :user :content emoji :timestamp 1}]}
        request  (sut/build-request env google-model context {} false)
        payload  (json/read-str (:body request) {:key-fn keyword})
        usr-text (get-in payload [:contents 0 :parts 0 :text])]
    (is (str/includes? (str usr-text) (String. (char-array [(char 0xD83D) (char 0xDE48)])))
        "valid emoji surrogate pair should be preserved")))
