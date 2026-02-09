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
    {:json/encode      json/write-str
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
  (let [context  (fixture "request_context")
        request  (sut/build-request (stub-env) google-model context {:max-output-tokens 256 :temperature 0.25} false)
        payload  (json/read-str (:body request) {:key-fn keyword})
        contents (:contents payload)]
    (is (= :post (:method request)))
    (is (= "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent" (:url request)))
    (is (= "google-key" (get-in request [:headers "x-goog-api-key"])))
    (is (= "Please use the tool for value 21." (get-in contents [0 :parts 0 :text])))
    (is (= "image/png" (get-in contents [0 :parts 1 :inlineData :mimeType])))
    (is (= true (get-in contents [1 :parts 0 :thought])))
    (is (= "Need tool." (get-in contents [1 :parts 0 :text])))
    (is (= "c2lnX3RleHQ=" (get-in contents [1 :parts 1 :thoughtSignature])))
    (is (= "double_number" (get-in contents [1 :parts 2 :functionCall :name])))
    (is (= {:value 21} (get-in contents [1 :parts 2 :functionCall :args])))
    (is (= "42" (get-in contents [2 :parts 0 :functionResponse :response :output])))
    (is (= 256 (get-in payload [:generationConfig :maxOutputTokens])))
    (is (= 0.25 (get-in payload [:generationConfig :temperature])))
    (is (= "You are a helpful assistant." (:systemInstruction payload)))
    (is (= "double_number" (get-in payload [:tools 0 :functionDeclarations 0 :name])))))

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
        multimodal-request (sut/build-request (stub-env) google-gemini3-model context {} false)
        multimodal-payload (json/read-str (:body multimodal-request) {:key-fn keyword})
        text-only-request  (sut/build-request (stub-env) google-text-only-model context {} false)
        text-only-payload  (json/read-str (:body text-only-request) {:key-fn keyword})]
    (is (= "image/png" (get-in multimodal-payload [:contents 1 :parts 0 :functionResponse :parts 0 :inlineData :mimeType])))
    (is (= "(see attached image)" (get-in multimodal-payload [:contents 1 :parts 0 :functionResponse :response :output])))
    (is (nil? (get-in text-only-payload [:contents 1 :parts 0 :functionResponse :parts])))
    (is (= "" (get-in text-only-payload [:contents 1 :parts 0 :functionResponse :response :output])))))

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
    (is (= [:thinking-start
            :thinking-delta
            :thinking-end
            :text-start
            :text-delta
            :text-end
            :toolcall-start
            :toolcall-delta
            :toolcall-end]
           (mapv :type (:events reduced))))
    (is (= "thinking one" (get-in finalized [:assistant-message :content 0 :thinking])))
    (is (= " text one" (get-in finalized [:assistant-message :content 1 :text])))
    (is (= {:value 21} (get-in finalized [:assistant-message :content 2 :arguments])))
    (is (= :tool-use (get-in finalized [:assistant-message :stop-reason])))
    (is (= {:input 12 :output 5 :cache-read 2 :cache-write 0 :total-tokens 19}
           (select-keys (get-in finalized [:assistant-message :usage])
                        [:input :output :cache-read :cache-write :total-tokens])))))

(deftest thought-signature-alone-is-not-thinking
  (let [env    (stub-env)
        state  {:model google-model}
        chunk  {:candidates [{:content {:parts [{:text "plain text" :thoughtSignature "opaque"}]}}]}
        result (sut/decode-event env state (json/write-str chunk))]
    (is (= [:text-start :text-delta]
           (mapv :type (:events result))))
    (is (= :text (get-in result [:state :current-block :kind])))
    (is (= "opaque" (get-in result [:state :assistant-message :content 0 :signature])))))

(deftest decode-event-tool-call-missing-args-defaults-to-empty-map
  (let [env    (stub-env)
        chunk  {:candidates [{:content      {:parts [{:functionCall {:name "get_status"}}]}
                              :finishReason "STOP"}]}
        result (sut/decode-event env {:model google-model} (json/write-str chunk))
        done   (sut/finalize env (:state result))]
    (is (= [:toolcall-start :toolcall-delta :toolcall-end]
           (mapv :type (:events result))))
    (is (= "get_status" (get-in done [:assistant-message :content 0 :name])))
    (is (= {} (get-in done [:assistant-message :content 0 :arguments])))
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
    (is (= :error (:stop-reason out)))
    (is (= [{:type :text :text "partial"}] (:content out)))
    (is (string? (:error-message out)))
    (is (.contains ^String (:error-message out) "bad gateway"))))

(deftest map-stop-reason-fail-fast-on-unknown
  (testing "unknown finish reason in stream decode fails fast"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown Google finish reason"
         (sut/decode-event
          (stub-env)
          {:model google-model}
          (json/write-str {:candidates [{:finishReason "UNRECOGNIZED_REASON"}]}))))))
