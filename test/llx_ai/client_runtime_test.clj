(ns llx-ai.client-runtime-test
  (:require
   [babashka.json :as json]
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.runtime :as sut]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.utils.unicode :as unicode]))

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

(defn- valid-assistant
  [timestamp]
  {:role        :assistant
   :content     [{:type :text :text "ok"}]
   :api         :openai-completions
   :provider    :openai
   :model       "gpt-4o-mini"
   :usage       {:input        1
                 :output       1
                 :cache-read   0
                 :cache-write  0
                 :total-tokens 2
                 :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
   :stop-reason :stop
   :timestamp   timestamp})

(defn- stub-env
  []
  {:http/request             (fn [_request] {:status 200 :body nil})
   :json/encode              json/write-str
   :json/decode              (fn [s _opts] (json/read-str s {:key-fn keyword}))
   :clock/now-ms             (constantly 1730000000000)
   :id/new                   (constantly "id-1")
   :http/read-body-string    (fn [_body] "")
   :unicode/sanitize-payload unicode/sanitize-payload})

(defn- take-events-with-timeout
  [stream timeout-ms]
  (loop [events []]
    (if-let [event (event-stream/take! stream timeout-ms)]
      (recur (conj events event))
      events)))

(defn- make-normalize-error-fn
  []
  (fn [env ex _partial-state]
    {:role          :assistant
     :content       []
     :api           :openai-completions
     :provider      :openai
     :model         "gpt-4o-mini"
     :usage         {:input        0
                     :output       0
                     :cache-read   0
                     :cache-write  0
                     :total-tokens 0
                     :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
     :stop-reason   :error
     :error-message (or (ex-message ex) "runtime failure")
     :timestamp     ((:clock/now-ms env))}))

(deftest run-stream-validation
  (testing "rejects malformed decoded events"
    (let [line    (str "data: " (json/write-str {:chunk true}) "\n" "data: [DONE]\n")
          body    (java.io.ByteArrayInputStream. (.getBytes line "UTF-8"))
          out     (event-stream/assistant-message-stream)
          state*  (atom {:model base-model})
          adapter {:api             :openai-completions
                   :build-request   (fn [_env _model _context _opts _stream?]
                                      {:method :post :url "https://example.invalid"})
                   :open-stream     (fn [_env _model _request] {:body body})
                   :decode-event    (fn [_env state _payload]
                                      {:state  state
                                       :events [{:type :toolcall-delta :id "call_1" :name "search"}]})
                   :finalize        (fn [_env _state]
                                      {:assistant-message (valid-assistant 1730000000000)
                                       :events            []})
                   :normalize-error (make-normalize-error-fn)}]
      (sut/run-stream! {:adapter adapter
                        :env     (stub-env)
                        :model   base-model
                        :request {:method :post :url "https://example.invalid"}
                        :out     out
                        :state*  state*})
      (let [events (event-stream/drain! out)
            result (event-stream/result out)]
        (is (= :error (:type (last events))))
        (is (= :error (:stop-reason result)))
        (is (string? (:error-message result)))
        (is (re-find #"Schema validation failed" (or (:error-message result) ""))))))
  (testing "rejects malformed finalize output"
    (let [line    "data: [DONE]\n"
          body    (java.io.ByteArrayInputStream. (.getBytes line "UTF-8"))
          out     (event-stream/assistant-message-stream)
          state*  (atom {:model base-model})
          adapter {:api             :openai-completions
                   :build-request   (fn [_env _model _context _opts _stream?]
                                      {:method :post :url "https://example.invalid"})
                   :open-stream     (fn [_env _model _request] {:body body})
                   :decode-event    (fn [_env state _payload] {:state state :events []})
                   :finalize        (fn [_env _state] {:assistant-message {:role :assistant}
                                                       :events            []})
                   :normalize-error (make-normalize-error-fn)}]
      (sut/run-stream! {:adapter adapter
                        :env     (stub-env)
                        :model   base-model
                        :request {:method :post :url "https://example.invalid"}
                        :out     out
                        :state*  state*})
      (let [events (event-stream/drain! out)
            result (event-stream/result out)]
        (is (= :error (:type (last events))))
        (is (= :error (:stop-reason result)))
        (is (string? (:error-message result)))
        (is (re-find #"Schema validation failed" (or (:error-message result) ""))))))
  (testing "malformed normalize-error still emits terminal error"
    (let [out     (event-stream/assistant-message-stream)
          state*  (atom {:model base-model})
          adapter {:api             :openai-completions
                   :build-request   (fn [_env _model _context _opts _stream?]
                                      {:method :post :url "https://example.invalid"})
                   :open-stream     (fn [_env _model _request]
                                      (throw (ex-info "stream boom" {:status 500})))
                   :decode-event    (fn [_env state _payload] {:state state :events []})
                   :finalize        (fn [_env _state] {:assistant-message (valid-assistant 1730000000000)
                                                       :events            []})
                   :normalize-error (fn [_env _ex _partial-state]
                                      {:role :assistant})}]
      (sut/run-stream! {:adapter adapter
                        :env     (stub-env)
                        :model   base-model
                        :request {:method :post :url "https://example.invalid"}
                        :out     out
                        :state*  state*})
      (let [events (take-events-with-timeout out 1000)
            result (deref (:result* out) 1000 ::timeout)]
        (is (seq events))
        (is (= :error (:type (last events))))
        (is (not= ::timeout result))
        (is (= :error (:stop-reason result)))
        (is (string? (:error-message result)))
        (is (re-find #"normalize-error failure" (:error-message result)))))))
