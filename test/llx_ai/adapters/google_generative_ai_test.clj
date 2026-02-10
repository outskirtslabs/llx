(ns llx-ai.adapters.google-generative-ai-test
  (:require
   [babashka.json :as json]
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [llx-ai.adapters.google-generative-ai :as sut]))

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
    {:http/request     (fn [_] {:status 200 :body "{}"})
     :json/encode      json/write-str
     :json/decode      (fn [s _opts] (json/read-str s {:key-fn keyword}))
     :json/decode-safe (fn [s _opts]
                         (try
                           (json/read-str s {:key-fn keyword})
                           (catch Exception _ nil)))
     :clock/now-ms     (fn [] 1730000000000)
     :id/new           (fn [] "generated-id")
     :env/get          (fn [k]
                         (case k
                           "GEMINI_API_KEY" "google-key"
                           nil))}
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
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Google generative ai request failed"
       (sut/open-stream
        (stub-env {:http/request (fn [_]
                                   {:status 401
                                    :body   (json/write-str {:error {:message "bad key"}})})})
        google-model
        {:method :post :url "https://example.invalid"}))))

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

(deftest map-stop-reason-fail-fast-on-unknown
  (testing "unknown finish reason in stream decode fails fast"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown Google finish reason"
         (sut/decode-event
          (stub-env)
          {:model google-model}
          (json/write-str {:candidates [{:finishReason "UNRECOGNIZED_REASON"}]}))))))
