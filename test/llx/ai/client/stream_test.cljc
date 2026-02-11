(ns llx.ai.client.stream-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]
             [llx.ai.impl.client.stream :as stream]
             [llx.ai.impl.schema :as schema]
             [llx.ai.test-util :as util]
             [promesa.core :as p]
             [promesa.exec.csp :as sp]]
       :cljs [[cljs.test :refer [deftest is]]])))

#?(:clj
   (do
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
       []
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
        :timestamp   1730000000000})

     (defn- base-env
       []
       {:http/request             (fn [_request] {:status 200 :body nil})
        :json/encode              pr-str
        :json/decode              (fn [_s _opts] {})
        :json/decode-safe         (fn [_s _opts] nil)
        :clock/now-ms             (constantly 1730000000000)
        :id/new                   (constantly "id-1")
        :http/read-body-string    (fn [_body] "")
        :unicode/sanitize-payload identity
        :thread/sleep             (fn [ms]
                                    (p/delay (long (max 0 (or ms 0))) nil))})

     (defn- base-adapter
       []
       {:api             :openai-completions
        :build-request   (fn [_env _model _context _opts _stream?]
                           {:method :post :url "https://example.invalid"})
        :open-stream     (fn [_env _model _request]
                           {:status 200})
        :decode-event    (fn [_env state _payload]
                           {:state state :events []})
        :finalize        (fn [_env _state]
                           {:assistant-message (valid-assistant)
                            :events            []})
        :normalize-error (fn [_env ex _partial-state]
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
                                            :cost         {:input 0.0
                                                           :output 0.0
                                                           :cache-read 0.0
                                                           :cache-write 0.0
                                                           :total 0.0}}
                            :stop-reason   :error
                            :error-message (or (ex-message ex) "runtime failure")
                            :timestamp     1730000000000})})

     (defn- base-run-stream-input
       [out]
       {:adapter (base-adapter)
        :env     (base-env)
        :model   base-model
        :request {:method :post :url "https://example.invalid"}
        :out     out
        :state*  (atom {:model base-model})})

     (defn- wait-until
       [pred timeout-ms interval-ms]
       (p/loop [elapsed 0]
         (cond
           (pred) true
           (>= elapsed timeout-ms) false
           :else (p/let [_ (p/delay interval-ms nil)]
                   (p/recur (+ elapsed interval-ms))))))

     (deftest runtime-run-stream-input-requires-runtime-hooks
       (let [out       (sp/chan)
             base-args (base-run-stream-input out)]
         (testing "requires stream runtime hooks"
           (is (not (schema/valid? :llx/runtime-run-stream-input base-args)))
           (is (schema/valid? :llx/runtime-run-stream-input
                              (assoc base-args
                                     :open-stream! (fn [] (p/resolved {:status 200}))
                                     :start-source! (fn [_args] {:stop-fn (fn [])})))))
         (sp/close out)))

     (deftest run-stream-calls-cancel-when-output-channel-closes
       (let [out        (sp/chan)
             cancelled* (atom 0)
             started*   (atom 0)
             _          (sp/close out)
             runtime    (stream/run-stream!
                         (assoc (base-run-stream-input out)
                                :cancel! (fn []
                                           (swap! cancelled* inc))
                                :open-stream! (fn []
                                                (p/resolved {:status 200}))
                                :start-source! (fn [{:keys [payload-ch]}]
                                                 (swap! started* inc)
                                                 (sp/close payload-ch)
                                                 {:stop-fn (fn []
                                                             (sp/close payload-ch))})))]
         (is (true? (stream/await! (wait-until #((:done? runtime)) 1000 10))))
         (is (= 1 @cancelled*))
         (is (= 0 @started*))))

     (deftest run-stream-emits-start-and-done-events
       (let [out      (sp/chan)
             _runtime (stream/run-stream!
                       (assoc (base-run-stream-input out)
                              :open-stream! (fn []
                                              (p/resolved {:status 200}))
                              :start-source! (fn [{:keys [payload-ch]}]
                                               (sp/close payload-ch)
                                               {:stop-fn (fn []
                                                           (sp/close payload-ch))})))
             events   (util/collect-channel-events! out 1000)]
         (is (= :start (:type (first events))))
         (is (= :done (:type (last events))))))))

#?(:cljs
   (deftest stream-test-placeholder
     (is true)))
