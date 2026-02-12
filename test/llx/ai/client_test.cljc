(ns llx.ai.client-test
  (:require
   [clojure.string :as str]
   #?@(:clj [[clojure.test :refer [deftest is]]
             [llx.ai.impl.client.jvm :as runtime]]
       :cljs [[cljs.test :refer-macros [deftest is]]
              [llx.ai.impl.client.node :as runtime]])
   [llx.ai.impl.client :as sut]
   [llx.ai.impl.registry :as registry]
   [llx.ai.impl.utils.unicode :as unicode]
   #?@(:clj [[llx.ai.test-util :as util]]
       :cljs [[llx.ai.test-util :as util :include-macros true]])
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

#?(:clj (set! *warn-on-reflection* true))

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

(defn- stub-env
  [handler]
  {:http/request             handler
   :json/encode              util/json-write
   :json/decode              (fn [s opts] (util/json-read s opts))
   :json/decode-safe         (fn [s opts] (util/json-read-safe s opts))
   :http/read-body-string    (fn [body]
                               (if (string? body)
                                 body
                                 (str body)))
   :stream/run!              runtime/run-stream!
   :registry                 sut/default-registry
   :clock/now-ms             (fn [] 1730000000000)
   :id/new                   (fn [] "id-1")
   :thread/sleep             #?(:clj (fn [_ms])
                                :cljs (fn [ms]
                                        (p/delay (long (max 0 (or ms 0))) nil)))
   :unicode/sanitize-payload unicode/sanitize-payload})

#?(:clj
   (defn- sse-body
     [events]
     (let [payload (str (str/join "\n" events) "\n")]
       (java.io.ByteArrayInputStream. (.getBytes payload "UTF-8"))))
   :cljs
   (defn- sse-body
     [events]
     (let [payload (str (str/join "\n" events) "\n")
           encoder (js/TextEncoder.)
           sent?   (atom false)]
       #js {:getReader (fn []
                         #js {:read   (fn []
                                        (if @sent?
                                          (js/Promise.resolve #js {:done true})
                                          (do
                                            (reset! sent? true)
                                            (js/Promise.resolve
                                             #js {:done  false
                                                  :value (.encode encoder payload)}))))
                              :cancel (fn []
                                        (js/Promise.resolve nil))})})))

(defn- rejects-with!
  [deferred assert-ex! done]
  (-> deferred
      (p/then (fn [_]
                (throw (ex-info "Expected rejection" {:type :llx/test-expected-rejection}))))
      (p/catch (fn [ex]
                 (assert-ex! ex)
                 true))
      (p/then (fn [_]
                (done)))
      (p/catch (partial util/fail-and-done! done))))

(deftest complete-returns-deferred
  (util/async done
              (let [env   (stub-env (fn [_]
                                      {:status 200
                                       :body   (util/json-write
                                                {:choices [{:finish_reason "stop"
                                                            :message       {:role "assistant" :content "ok"}}]
                                                 :usage   {:prompt_tokens 1 :completion_tokens 1 :total_tokens 2}})}))
                    out-d (sut/complete* env base-model {:messages [{:role :user :content "hi" :timestamp 1}]} {:api-key "x"})]
                (is (p/deferred? out-d))
                (-> out-d
                    (p/then (fn [out]
                              (is (= :assistant (:role out)))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest stream-returns-csp-channel
  (util/async done
              (let [env (stub-env (fn [_]
                                    {:status 200
                                     :body   (sse-body
                                              [(str "data: " (util/json-write {:choices [{:delta         {:content "ok"}
                                                                                          :finish_reason "stop"}]
                                                                               :usage   {:prompt_tokens     1
                                                                                         :completion_tokens 1
                                                                                         :total_tokens      2}}))
                                               "data: [DONE]"])}))
                    ch  (sut/stream* env base-model {:messages [{:role :user :content "hi" :timestamp 1}]} {:api-key "x"})]
                (is (sp/chan? ch))
                (-> (util/collect-stream* ch 1000)
                    (p/then (fn [{:keys [events]}]
                              (is (= :done (:type (last events))))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest unified-opts->request-opts-normalizes-unified-shape
  (let [model          (assoc base-model :max-tokens 64000)
        response-model {:id             "gpt-5-mini"
                        :name           "GPT-5 Mini"
                        :provider       :openai
                        :api            :openai-responses
                        :base-url       "https://api.openai.com/v1"
                        :context-window 400000
                        :max-tokens     128000
                        :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                        :capabilities   {:reasoning? true :input #{:text}}}]
    (is (= {:max-output-tokens 99
            :temperature       0.2
            :reasoning         {:level :high}}
           (sut/unified-opts->request-opts model
                                           {:max-tokens 99 :reasoning :xhigh :temperature 0.2})))
    (is (= {:max-output-tokens 32000}
           (sut/unified-opts->request-opts model {})))
    (is (= {:max-output-tokens 77
            :reasoning         {:effort :high}}
           (sut/unified-opts->request-opts response-model
                                           {:max-tokens 77 :reasoning :high})))))

(deftest unified-opts->request-opts-rejects-provider-shape-in-unified-opts
  (let [ex (try
             (sut/unified-opts->request-opts base-model
                                             {:reasoning {:level :high}})
             nil
             (catch #?(:clj Exception :cljs :default) e
               e))]
    (is (some? ex))))

(deftest complete-simple-wrapper-normalizes-options
  (util/async done
              (let [called*   (atom nil)
                    env       (stub-env (fn [_] {:status 200 :body "{}"}))
                    model     (assoc base-model :max-tokens 64000)
                    context   {:messages [{:role :user :content "hi" :timestamp 1}]}
                    sentinel  {:role        :assistant
                               :content     []
                               :api         (:api model)
                               :provider    (:provider model)
                               :model       (:id model)
                               :usage       {:input        0
                                             :output       0
                                             :cache-read   0
                                             :cache-write  0
                                             :total-tokens 0
                                             :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                               :stop-reason :stop
                               :timestamp   0}
                    simple-in {:max-tokens 99 :reasoning :xhigh :temperature 0.2}]
                (-> (with-redefs [sut/complete* (fn [env' model' context' opts']
                                                  (reset! called* {:env env' :model model' :context context' :opts opts'})
                                                  (p/resolved sentinel))]
                      (sut/complete env model context simple-in))
                    (p/then (fn [out]
                              (is (= sentinel out))
                              (is (= {:max-output-tokens 99
                                      :temperature       0.2
                                      :reasoning         {:level :high}}
                                     (:opts @called*)))
                              (with-redefs [sut/complete* (fn [_env _model _context opts']
                                                            (reset! called* opts')
                                                            (p/resolved sentinel))]
                                (sut/complete env model context {}))))
                    (p/then (fn [_]
                              (is (= {:max-output-tokens 32000} @called*))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest stream-simple-wrapper-normalizes-options
  (let [called*   (atom nil)
        env       (stub-env (fn [_] {:status 200 :body "{}"}))
        model     (assoc base-model :max-tokens 64000)
        context   {:messages [{:role :user :content "hi" :timestamp 1}]}
        stream-st (sp/chan)
        simple-in {:max-tokens 77 :reasoning-effort :medium :top-p 0.9}]
    (with-redefs [sut/stream* (fn [env' model' context' opts']
                                (reset! called* {:env env' :model model' :context context' :opts opts'})
                                stream-st)]
      (is (identical? stream-st (sut/stream env model context simple-in)))
      (is (= {:max-output-tokens 77
              :top-p             0.9
              :reasoning         {:level :medium}}
             (:opts @called*))))))

(deftest complete-simple-wrapper-normalizes-openai-responses-reasoning-to-effort
  (util/async done
              (let [called*   (atom nil)
                    env       (stub-env (fn [_] {:status 200 :body "{}"}))
                    model     {:id             "gpt-5-mini"
                               :name           "GPT-5 Mini"
                               :provider       :openai
                               :api            :openai-responses
                               :base-url       "https://api.openai.com/v1"
                               :context-window 400000
                               :max-tokens     128000
                               :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
                               :capabilities   {:reasoning? true :input #{:text}}}
                    context   {:messages [{:role :user :content "hi" :timestamp 1}]}
                    sentinel  {:role        :assistant
                               :content     []
                               :api         (:api model)
                               :provider    (:provider model)
                               :model       (:id model)
                               :usage       {:input        0
                                             :output       0
                                             :cache-read   0
                                             :cache-write  0
                                             :total-tokens 0
                                             :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                               :stop-reason :stop
                               :timestamp   0}
                    simple-in {:max-tokens 77 :reasoning :high :top-p 0.9}]
                (-> (with-redefs [sut/complete* (fn [_env _model _context opts']
                                                  (reset! called* opts')
                                                  (p/resolved sentinel))]
                      (sut/complete env model context simple-in))
                    (p/then (fn [out]
                              (is (= sentinel out))
                              (is (= {:max-output-tokens 77
                                      :top-p             0.9
                                      :reasoning         {:effort :high}}
                                     @called*))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest complete-openai-completions-non-2xx
  (util/async done
              (let [env (stub-env (fn [_request]
                                    {:status 401
                                     :body   (util/json-write {:error {:message "bad key"}})}))
                    d   (sut/complete* env base-model {:messages [{:role :user :content "hi" :timestamp 1}]} {:api-key "bad"})]
                (rejects-with! d
                               (fn [ex]
                                 (is (= {:type         :llx/authentication-error
                                         :message      "bad key"
                                         :recoverable? false
                                         :provider     "openai"
                                         :http-status  401}
                                        (ex-data ex))))
                               done))))

(deftest complete-emits-lifecycle-trove-signals
  (util/async done
              (-> (util/with-captured-logs!
                    (fn [logs*]
                      (let [env     (stub-env (fn [_request]
                                                {:status 200
                                                 :body   (util/json-write
                                                          {:choices [{:finish_reason "stop"
                                                                      :message       {:role "assistant" :content "ok"}}]
                                                           :usage   {:prompt_tokens 1 :completion_tokens 1 :total_tokens 2}})}))
                            context {:messages [{:role :user :content "hi" :timestamp 1}]}]
                        (-> (sut/complete* env base-model context {:api-key "x"})
                            (p/then (fn [_]
                                      (let [start      (util/first-event logs* :llx.obs/call-start)
                                            done-event (util/first-event logs* :llx.obs/call-finished)]
                                        (is (util/submap?
                                             {:id    :llx.obs/call-start
                                              :level :info
                                              :data  {:operation :complete
                                                      :model-id  "gpt-4o-mini"}}
                                             (util/strip-generated start [:call-id :provider :api :message-count :has-tools? :has-system-prompt?])))
                                        (is (util/submap?
                                             {:id    :llx.obs/call-finished
                                              :level :info
                                              :data  {:operation   :complete
                                                      :stop-reason :stop}}
                                             (util/strip-generated done-event [:call-id :provider :api :model-id :usage :content-block-count])))
                                        true)))))))
                  (p/then (fn [_]
                            (done)))
                  (p/catch (partial util/fail-and-done! done)))))

(deftest complete-emits-error-lifecycle-trove-signals
  (util/async done
              (-> (util/with-captured-logs!
                    (fn [logs*]
                      (let [env     (stub-env (fn [_request]
                                                {:status 401
                                                 :body   (util/json-write {:error {:message "bad key"}})}))
                            context {:messages [{:role :user :content "hi" :timestamp 1}]}]
                        (-> (sut/complete* env base-model context {:api-key "bad"})
                            (p/then (fn [_]
                                      (throw (ex-info "Expected rejection" {:type :llx/test-expected-rejection}))))
                            (p/catch (fn [_]
                                       (let [status (util/first-event logs* :llx.obs/http-status-error)
                                             failed (util/first-event logs* :llx.obs/call-error)]
                                         (is (util/submap?
                                              {:id    :llx.obs/http-status-error
                                               :level :info
                                               :data  {:status 401}}
                                              (util/strip-generated status [:call-id :operation :provider :api :model-id :request-id :provider-code :retry-after :error-type])))
                                         (is (util/submap?
                                              {:id    :llx.obs/call-error
                                               :level :error
                                               :data  {:operation  :complete
                                                       :error-type :llx/authentication-error}}
                                              (util/strip-generated failed [:call-id :provider :api :model-id :error-message :recoverable? :request-id :provider-code])))
                                         true)))))))
                  (p/then (fn [_]
                            (done)))
                  (p/catch (partial util/fail-and-done! done)))))

(deftest opts-registry-overrides-env-registry
  (util/async done
              (let [seen-request (atom nil)
                    env          (stub-env (fn [request]
                                             (reset! seen-request request)
                                             {:status 200
                                              :body   (util/json-write
                                                       {:choices [{:finish_reason "stop"
                                                                   :message       {:role    "assistant"
                                                                                   :content "override ok"}}]
                                                        :usage   {:prompt_tokens     1
                                                                  :completion_tokens 1
                                                                  :total_tokens      2}})}))
                    env          (assoc env :registry (registry/immutable-registry))]
                (-> (sut/complete* env base-model {:messages [{:role :user :content "ping" :timestamp 1}]}
                                   {:api-key  "x"
                                    :registry sut/default-registry})
                    (p/then (fn [out]
                              (is (= "override ok" (get-in out [:content 0 :text])))
                              (is (= "gpt-4o-mini" (get-in (util/json-read (:body @seen-request) {:key-fn keyword}) [:model])))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest complete-transforms-context-before-build-request
  (util/async done
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
                                                                                     :usage       {:input        0
                                                                                                   :output       0
                                                                                                   :cache-read   0
                                                                                                   :cache-write  0
                                                                                                   :total-tokens 0
                                                                                                   :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
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
                                                  :usage       {:input        1
                                                                :output       1
                                                                :cache-read   0
                                                                :cache-write  0
                                                                :total-tokens 2
                                                                :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                                                  :stop-reason :tool-use
                                                  :timestamp   2}
                                                 {:role :user :content "continue" :timestamp 3}]}]
                (-> (sut/complete* env model context {})
                    (p/then (fn [_]
                              (is (= [:user :assistant :tool-result :user]
                                     (mapv :role (:messages @captured-context))))
                              (is (= "norm_call_1" (get-in @captured-context [:messages 1 :content 0 :id])))
                              (is (= "norm_call_1" (get-in @captured-context [:messages 2 :tool-call-id])))
                              (is (= true (get-in @captured-context [:messages 2 :is-error?])))
                              (is (= 4242 (get-in @captured-context [:messages 2 :timestamp])))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest complete-rejects-malformed-adapter-finalize-result
  (util/async done
              (let [adapter {:api             :openai-completions
                             :build-request   (fn [_env _model _context _opts _stream?]
                                                {:method :post :url "https://example.invalid" :headers {} :body "{}"})
                             :open-stream     (fn [_env _model _request] nil)
                             :decode-event    (fn [_env state _chunk] {:state state :events []})
                             :finalize        (fn [_env _state] {:assistant-message {:role :assistant}
                                                                 :events            []})
                             :normalize-error (fn [_env _ex _partial] {})
                             :supports-model? (fn [_model] true)}
                    reg     (registry/register-adapter (registry/immutable-registry) adapter "test")
                    env     (-> (stub-env (fn [_request] {:status 200 :body "{}"}))
                                (assoc :registry reg))
                    context {:messages [{:role :user :content "hello" :timestamp 1}]}
                    d       (sut/complete* env base-model context {:api-key "x"})]
                (rejects-with! d
                               (fn [ex]
                                 (is (re-find #"Schema validation failed" (or (ex-message ex) ""))))
                               done))))

(deftest complete-fails-fast-on-unknown-openai-completions-stop-reason
  (util/async done
              (let [env     (stub-env (fn [_request]
                                        {:status 200
                                         :body   (util/json-write
                                                  {:choices [{:finish_reason "unknown_new_reason"
                                                              :message       {:role    "assistant"
                                                                              :content "ok"}}]
                                                   :usage   {:prompt_tokens 2 :completion_tokens 1 :total_tokens 3}})}))
                    context {:messages [{:role :user :content "hello" :timestamp 1}]}
                    d       (sut/complete* env base-model context {:api-key "x"})]
                (rejects-with! d
                               (fn [ex]
                                 (is (re-find #"Unknown OpenAI completions stop reason"
                                              (or (ex-message ex) ""))))
                               done))))

(deftest complete-retries-on-transient-error
  (util/async done
              (let [call-count (atom 0)
                    env        (stub-env (fn [_request]
                                           (swap! call-count inc)
                                           (if (= 1 @call-count)
                                             {:status 503
                                              :body   (util/json-write {:error {:message "service unavailable"}})}
                                             {:status 200
                                              :body   (util/json-write
                                                       {:choices [{:finish_reason "stop"
                                                                   :message       {:role    "assistant"
                                                                                   :content "ok"}}]
                                                        :usage   {:prompt_tokens     1
                                                                  :completion_tokens 1
                                                                  :total_tokens      2}})})))
                    context    {:messages [{:role :user :content "hi" :timestamp 1}]}]
                (-> (sut/complete* env base-model context {:api-key "x" :max-retries 2})
                    (p/then (fn [out]
                              (is (= 2 @call-count))
                              (is (= :stop (:stop-reason out)))
                              (is (= [{:type :text :text "ok"}] (:content out)))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest complete-does-not-retry-on-client-error
  (util/async done
              (let [call-count (atom 0)
                    env        (stub-env (fn [_request]
                                           (swap! call-count inc)
                                           {:status 401
                                            :body   (util/json-write {:error {:message "unauthorized"}})}))
                    context    {:messages [{:role :user :content "hi" :timestamp 1}]}
                    d          (sut/complete* env base-model context {:api-key "bad" :max-retries 2})]
                (rejects-with! d
                               (fn [ex]
                                 (is (= {:type         :llx/authentication-error
                                         :message      "unauthorized"
                                         :recoverable? false
                                         :provider     "openai"
                                         :http-status  401}
                                        (ex-data ex)))
                                 (is (= 1 @call-count)))
                               done))))

(deftest complete-throws-config-error-when-retries-requested-without-sleep
  (util/async done
              (let [env-no-sleep (dissoc (stub-env (fn [_] {:status 200 :body "{}"})) :thread/sleep)
                    context      {:messages [{:role :user :content "hi" :timestamp 1}]}
                    d            (sut/complete* env-no-sleep base-model context {:api-key "x" :max-retries 2})]
                (rejects-with! d
                               (fn [ex]
                                 (is (= {:type        :llx/config-error
                                         :max-retries 2}
                                        (ex-data ex))))
                               done))))

(deftest complete-rejects-unsupported-xhigh-with-structured-error
  (util/async done
              (let [env     (stub-env (fn [_request]
                                        {:status 200
                                         :body   (util/json-write
                                                  {:choices [{:finish_reason "stop"
                                                              :message       {:role    "assistant"
                                                                              :content "should not reach"}}]
                                                   :usage   {:prompt_tokens 1 :completion_tokens 1 :total_tokens 2}})}))
                    context {:messages [{:role :user :content "hi" :timestamp 1}]}
                    d       (sut/complete* env base-model context
                                           {:api-key   "x"
                                            :reasoning {:level :xhigh}})]
                (rejects-with! d
                               (fn [ex]
                                 (is (= :llx/unsupported-reasoning-level (-> ex ex-data :type)))
                                 (is (= :xhigh (-> ex ex-data :requested-level)))
                                 (is (= "gpt-4o-mini" (-> ex ex-data :model-id)))
                                 (is (false? (-> ex ex-data :recoverable?))))
                               done))))

(deftest stream-rejects-unsupported-xhigh-with-structured-error
  (let [env     (stub-env (fn [_request]
                            {:status 200
                             :body   (sse-body ["data: [DONE]"])}))
        context {:messages [{:role :user :content "hi" :timestamp 1}]}
        ex      (try
                  (sut/stream* env base-model context
                               {:api-key   "x"
                                :reasoning {:level :xhigh}})
                  nil
                  (catch #?(:clj Exception :cljs :default) e
                    e))]
    (is (some? ex))
    (is (= :llx/unsupported-reasoning-level (-> ex ex-data :type)))))

(deftest complete-allows-xhigh-for-supported-model
  (util/async done
              (let [xhigh-model (assoc base-model
                                       :id "claude-opus-4-6"
                                       :api :anthropic-messages
                                       :provider :anthropic
                                       :capabilities {:reasoning? true :input #{:text}})
                    env         (stub-env (fn [_request]
                                            {:status 200
                                             :body   (util/json-write
                                                      {:content     [{:type "text" :text "ok"}]
                                                       :stop_reason "end_turn"
                                                       :usage       {:input_tokens 1 :output_tokens 1}})}))
                    context     {:messages [{:role :user :content "hi" :timestamp 1}]}]
                (-> (sut/complete* env xhigh-model context
                                   {:api-key   "x"
                                    :reasoning {:level :xhigh}})
                    (p/then (fn [out]
                              (is (= :assistant (:role out)))
                              (is (= :stop (:stop-reason out)))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest stream-retries-open-stream-on-transient-error
  #?(:clj
     (util/async done
                 (let [call-count (atom 0)
                       env        (stub-env (fn [_request]
                                              (swap! call-count inc)
                                              (if (= 1 @call-count)
                                                {:status 503
                                                 :body   (util/json-write {:error {:message "service unavailable"}})}
                                                {:status  200
                                                 :headers {"content-type" "text/event-stream"}
                                                 :body    (sse-body
                                                           ["data: {\"choices\":[{\"delta\":{\"role\":\"assistant\",\"content\":\"hi\"},\"index\":0}]}"
                                                            "data: {\"choices\":[{\"delta\":{\"content\":\" there\"},\"index\":0,\"finish_reason\":\"stop\"}]}"
                                                            "data: [DONE]"])})))
                       stream     (sut/stream* env base-model
                                               {:messages [{:role :user :content "hi" :timestamp 1}]}
                                               {:api-key "x" :max-retries 2})]
                   (-> (util/collect-stream* stream 1000)
                       (p/then (fn [{:keys [events result]}]
                                 (is (= 2 @call-count))
                                 (is (not (util/timeout-result? events)))
                                 (is (= :done (:type (last events))))
                                 (is (= :stop (:stop-reason result)))
                                 (done)))
                       (p/catch (partial util/fail-and-done! done)))))
     :cljs
     (is true)))

(deftest stream-close-delegates-to-runtime-cancel-fn
  (util/async done
              (let [cancel-count* (atom 0)
                    run-count*    (atom 0)
                    runtime-hook  (fn [_input]
                                    (swap! run-count* inc)
                                    {:cancel-fn (fn []
                                                  (swap! cancel-count* inc))})
                    env           (-> (stub-env (fn [_request]
                                                  {:status 200
                                                   :body   (sse-body ["data: [DONE]"])}))
                                      (assoc :stream/run! runtime-hook))
                    st            (sut/stream* env base-model
                                               {:messages [{:role :user :content "hi" :timestamp 1}]}
                                               {:api-key "x"})]
                (is (sp/chan? st))
                (sp/close st)
                (-> (p/delay 50 true)
                    (p/then (fn [_]
                              (is (= 1 @run-count*))
                              (is (= 1 @cancel-count*))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))
