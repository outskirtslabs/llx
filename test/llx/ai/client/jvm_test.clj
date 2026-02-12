(ns llx.ai.client.jvm-test
  (:require
   [babashka.json :as json]
   [clojure.test :refer [deftest is testing]]
   [llx.ai.impl.client.jvm :as sut]
   [llx.ai.test-util :as util]
   [llx.ai.impl.utils.unicode :as unicode]
   [promesa.exec.csp :as sp]))

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

(defn- collect-stream!
  [ch timeout-ms]
  (let [events (util/collect-channel-events! ch timeout-ms)]
    {:events events
     :result (:assistant-message (last events))}))

(defn- wait-until!
  [pred timeout-ms]
  (loop [elapsed 0]
    (cond
      (pred) true
      (>= elapsed timeout-ms) false
      :else (do
              (Thread/sleep 10)
              (recur (+ elapsed 10))))))

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
          out     (sp/chan)
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
      (let [{:keys [events result]} (collect-stream! out 1000)]
        (is (= :error (:type (last events))))
        (is (= :error (:stop-reason result)))
        (is (string? (:error-message result)))
        (is (re-find #"Schema validation failed" (or (:error-message result) ""))))))
  (testing "rejects malformed finalize output"
    (let [line    "data: [DONE]\n"
          body    (java.io.ByteArrayInputStream. (.getBytes line "UTF-8"))
          out     (sp/chan)
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
      (let [{:keys [events result]} (collect-stream! out 1000)]
        (is (= :error (:type (last events))))
        (is (= :error (:stop-reason result)))
        (is (string? (:error-message result)))
        (is (re-find #"Schema validation failed" (or (:error-message result) ""))))))
  (testing "malformed normalize-error still emits terminal error"
    (let [out     (sp/chan)
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
      (let [{:keys [events result]} (collect-stream! out 1000)]
        (is (seq events))
        (is (= :error (:type (last events))))
        (is (not= ::timeout result))
        (is (= :error (:stop-reason result)))
        (is (string? (:error-message result)))
        (is (re-find #"normalize-error failure" (:error-message result)))))))

(deftest run-stream-cancel-fn-closes-upstream-body
  (let [closed* (atom false)
        gate    (promise)
        body    (proxy [java.io.InputStream] []
                  (read
                    ([] (do
                          @gate
                          -1)))
                  (close
                    ([] (reset! closed* true)
                        (deliver gate true))))
        out     (sp/chan)
        state*  (atom {:model base-model})
        adapter {:api             :openai-completions
                 :build-request   (fn [_env _model _context _opts _stream?]
                                    {:method :post :url "https://example.invalid"})
                 :open-stream     (fn [_env _model _request]
                                    {:status 200 :body body})
                 :decode-event    (fn [_env state _payload]
                                    {:state state :events []})
                 :finalize        (fn [_env _state]
                                    {:assistant-message (valid-assistant 1730000000000)
                                     :events            []})
                 :normalize-error (make-normalize-error-fn)}
        runtime (sut/run-stream! {:adapter adapter
                                  :env     (stub-env)
                                  :model   base-model
                                  :request {:method :post :url "https://example.invalid"}
                                  :out     out
                                  :state*  state*})]
    ((:cancel-fn runtime))
    (is (true? (wait-until! #(boolean @closed*) 1000)))))

(deftest run-stream-emits-stream-lifecycle-trove-signals
  (util/with-captured-logs!
   (fn [logs*]
     (let [line    (str "data: " (json/write-str {:type "delta" :text "ok"}) "\n" "data: [DONE]\n")
           body    (java.io.ByteArrayInputStream. (.getBytes line "UTF-8"))
           out     (sp/chan)
           state*  (atom {:model base-model})
           adapter {:api             :openai-completions
                    :build-request   (fn [_env _model _context _opts _stream?]
                                       {:method :post :url "https://example.invalid"})
                    :open-stream     (fn [_env _model _request] {:body body})
                    :decode-event    (fn [_env state _payload]
                                       {:state  state
                                        :events [{:type :text-start}
                                                 {:type :text-delta :text "ok"}
                                                 {:type :text-end}]})
                    :finalize        (fn [_env _state]
                                       {:assistant-message (valid-assistant 1730000000000)
                                        :events            []})
                    :normalize-error (make-normalize-error-fn)}]
       (sut/run-stream! {:adapter      adapter
                         :env          (assoc (stub-env) :call/id "call_1")
                         :model        base-model
                         :request      {:method :post :url "https://example.invalid"}
                         :out          out
                         :state*       state*
                         :request-opts {}})
       (collect-stream! out 1000)
       (let [start (util/first-event logs* :llx.obs/stream-start)
             item  (util/first-event logs* :llx.obs/stream-item-received)
             done  (util/first-event logs* :llx.obs/stream-done)]
         (is (util/submap?
              {:id   :llx.obs/stream-start                                                                  :level :info
               :data {:call-id "call_1" :provider :openai :api :openai-completions :model-id "gpt-4o-mini"}}
              (util/strip-generated start)))
         (is (util/submap?
              {:id   :llx.obs/stream-item-received                                                                        :level :trace
               :data {:call-id    "call_1" :provider       :openai     :api   :openai-completions :model-id "gpt-4o-mini"
                      :item-index 0        :llx-event-type :text-start :done? false}}
              (util/strip-generated item)))
         (is (util/submap?
              {:id   :llx.obs/stream-done                                                                                 :level :info
               :data {:call-id     "call_1" :provider            :openai :api :openai-completions :model-id "gpt-4o-mini"
                      :stop-reason :stop    :content-block-count 1}}
              (util/strip-generated done [:usage]))))))))

(deftest run-stream-emits-stream-event-error-trove-signal
  (util/with-captured-logs!
   (fn [logs*]
     (let [out     (sp/chan)
           state*  (atom {:model base-model})
           adapter {:api             :openai-completions
                    :build-request   (fn [_env _model _context _opts _stream?]
                                       {:method :post :url "https://example.invalid"})
                    :open-stream     (fn [_env _model _request]
                                       (throw (ex-info "stream boom"
                                                       {:type :llx/streaming-error})))
                    :decode-event    (fn [_env state _payload] {:state state :events []})
                    :finalize        (fn [_env _state]
                                       {:assistant-message (valid-assistant 1730000000000)
                                        :events            []})
                    :normalize-error (make-normalize-error-fn)}]
       (sut/run-stream! {:adapter      adapter
                         :env          (assoc (stub-env) :call/id "call_2")
                         :model        base-model
                         :request      {:method :post :url "https://example.invalid"}
                         :out          out
                         :state*       state*
                         :request-opts {}})
       (collect-stream! out 1000)
       (let [event (util/first-event logs* :llx.obs/stream-event-error)]
         (is (util/submap?
              {:id   :llx.obs/stream-event-error                                                                                                  :level :error
               :data {:call-id                 "call_2"             :provider      :openai       :api :openai-completions :model-id "gpt-4o-mini"
                      :error-type              :llx/streaming-error :error-message "stream boom"
                      :normalize-error-failed? false}}
              (util/strip-generated event))))))))

(deftest run-stream-wraps-untyped-stream-errors-as-streaming-error
  (util/with-captured-logs!
   (fn [logs*]
     (let [out     (sp/chan)
           state*  (atom {:model base-model})
           adapter {:api             :openai-completions
                    :build-request   (fn [_env _model _context _opts _stream?]
                                       {:method :post :url "https://example.invalid"})
                    :open-stream     (fn [_env _model _request]
                                       (throw (ex-info "stream boom" {})))
                    :decode-event    (fn [_env state _payload] {:state state :events []})
                    :finalize        (fn [_env _state]
                                       {:assistant-message (valid-assistant 1730000000000)
                                        :events            []})
                    :normalize-error (make-normalize-error-fn)}]
       (sut/run-stream! {:adapter      adapter
                         :env          (assoc (stub-env) :call/id "call_3")
                         :model        base-model
                         :request      {:method :post :url "https://example.invalid"}
                         :out          out
                         :state*       state*
                         :request-opts {}})
       (collect-stream! out 1000)
       (let [event (util/first-event logs* :llx.obs/stream-event-error)]
         (is (util/submap?
              {:id   :llx.obs/stream-event-error
               :level :error
               :data {:call-id                 "call_3"
                      :provider                :openai
                      :api                     :openai-completions
                      :model-id                "gpt-4o-mini"
                      :error-type              :llx/streaming-error
                      :error-message           "stream boom"
                      :normalize-error-failed? false}}
              (util/strip-generated event))))))))
