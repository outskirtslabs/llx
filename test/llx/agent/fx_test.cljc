(ns llx.agent.fx-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]]
       :cljs [[cljs.test :refer-macros [deftest is]]])
   [llx.agent.fx :as fx]
   [llx.agent.schema :as schema]
   [llx.ai :as ai]
   [llx.ai.test-util :as util]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- zero-usage
  []
  {:input        0
   :output       0
   :cache-read   0
   :cache-write  0
   :total-tokens 0
   :cost         {:input 0 :output 0 :cache-read 0 :cache-write 0 :total 0}})

(defn- assistant-message
  [model text]
  {:role        :assistant
   :content     [{:type :text :text text}]
   :api         (:api model)
   :provider    (:provider model)
   :model       (:id model)
   :usage       (zero-usage)
   :stop-reason :stop
   :timestamp   1})

(defn- empty-queue
  []
  #?(:clj clojure.lang.PersistentQueue/EMPTY
     :cljs #queue []))

(defn- base-state
  [model messages thinking-level tools]
  {:llx.agent.loop/phase :llx.agent.loop/streaming
   :system-prompt        "sys"
   :model                model
   :thinking-level       thinking-level
   :tools                tools
   :messages             messages
   :stream-message       nil
   :pending-tool-calls   []
   :error                nil
   :steering-queue       (empty-queue)
   :follow-up-queue      (empty-queue)
   :steering-mode        :one-at-a-time
   :follow-up-mode       :one-at-a-time})

(defn- stream-with-events
  [events]
  (let [ch (sp/chan :buf 16)]
    (doseq [event events]
      (sp/offer ch event))
    (sp/close ch)
    ch))

(defn- read-signals*
  [ch]
  (util/collect-events* ch 2000))

(defn- test-env
  [state overrides]
  (merge {:state_          (atom state)
          :command>        (sp/chan)
          :events-mx>      (sp/mult :buf (sp/sliding-buffer 16))
          :schema-registry (schema/registry {})
          :convert-to-llm  identity
          :abort-signal    nil}
         overrides))

(defn- tool-call
  ([]
   (tool-call {:path "/tmp/input.txt"}))
  ([arguments]
   {:id        "tc-1"
    :name      "read_file"
    :arguments arguments}))

(defn- tool-spec
  []
  {:name         "read_file"
   :description  "Read a file from disk"
   :input-schema [:map [:path :string]]})

(defn- tool-env
  [model tools overrides]
  (test-env
   (base-state model [] :off tools)
   (merge {:abort-signal ::abort-signal}
          overrides)))

(defn- execute-tool-behaviors*
  [model]
  (let [success-seen*      (atom nil)
        validation-called* (atom false)
        success-call       (tool-call)
        missing-call       {:id "tc-1" :name "missing_tool" :arguments {:path "/tmp/input.txt"}}
        invalid-call       (tool-call {:path 123})
        success-env        (tool-env model
                                     [(assoc (tool-spec)
                                             :execute (fn [tool-call-id args abort-signal on-update]
                                                        (reset! success-seen* {:tool-call-id tool-call-id
                                                                               :args         args
                                                                               :abort-signal abort-signal})
                                                        (p/let [_ (on-update {:progress 50})]
                                                          {:content [{:type :text
                                                                      :text (str "read " (:path args))}]})))]
                                     {})
        missing-env        (tool-env model [] {})
        validation-env     (tool-env model
                                     [(assoc (tool-spec)
                                             :execute (fn [_tool-call-id _args _abort-signal _on-update]
                                                        (reset! validation-called* true)
                                                        {:content [{:type :text :text "never"}]}))]
                                     {})
        throw-env          (tool-env model
                                     [(assoc (tool-spec)
                                             :execute (fn [_tool-call-id _args _abort-signal _on-update]
                                                        (throw (ex-info "read failed" {:type :test/tool-failed}))))]
                                     {})
        malformed-env      (tool-env model
                                     [(assoc (tool-spec)
                                             :execute (fn [_tool-call-id _args _abort-signal _on-update]
                                                        {:content "not-a-vector"}))]
                                     {})
        stale-env-tools    (test-env
                            (base-state model
                                        []
                                        :off
                                        [(assoc (tool-spec)
                                                :execute (fn [_tool-call-id _args _abort-signal _on-update]
                                                           {:content [{:type :text :text "from state"}]}))])
                            {:tools        [(assoc (tool-spec)
                                                   :execute (fn [_tool-call-id _args _abort-signal _on-update]
                                                              {:content [{:type :text :text "from env"}]}))]
                             :abort-signal ::abort-signal})]
    (p/let [success-signals    (read-signals* (fx/execute-fx success-env {:llx.agent.fx/type :execute-tool
                                                                          :tool-call         success-call}))
            missing-signals    (read-signals* (fx/execute-fx missing-env {:llx.agent.fx/type :execute-tool
                                                                          :tool-call         missing-call}))
            validation-signals (read-signals* (fx/execute-fx validation-env {:llx.agent.fx/type :execute-tool
                                                                             :tool-call         invalid-call}))
            throw-signals      (read-signals* (fx/execute-fx throw-env {:llx.agent.fx/type :execute-tool
                                                                        :tool-call         success-call}))
            malformed-signals  (read-signals* (fx/execute-fx malformed-env {:llx.agent.fx/type :execute-tool
                                                                            :tool-call         success-call}))
            stale-env-signals  (read-signals* (fx/execute-fx stale-env-tools {:llx.agent.fx/type :execute-tool
                                                                              :tool-call         success-call}))]
      {:success-seen       @success-seen*
       :validation-called? @validation-called*
       :success-signals    success-signals
       :missing-signals    missing-signals
       :validation-signals validation-signals
       :throw-signals      throw-signals
       :malformed-signals  malformed-signals
       :stale-env-signals  stale-env-signals})))

(defn- assert-execute-tool-behaviors!
  [{:keys [success-seen
           validation-called?
           success-signals
           missing-signals
           validation-signals
           throw-signals
           malformed-signals
           stale-env-signals]}]
  (is (not (util/timeout-result? success-signals)))
  (is (not (util/timeout-result? missing-signals)))
  (is (not (util/timeout-result? validation-signals)))
  (is (not (util/timeout-result? throw-signals)))
  (is (not (util/timeout-result? malformed-signals)))
  (is (not (util/timeout-result? stale-env-signals)))

  (is (= [:llx.agent.signal/tool-update
          :llx.agent.signal/tool-result]
         (mapv :type success-signals)))
  (is (= "tc-1" (get-in success-signals [0 :tool-call-id])))
  (is (= "read_file" (get-in success-signals [0 :tool-name])))
  (is (= {:progress 50} (get-in success-signals [0 :partial-result])))
  (is (= {:tool-call-id "tc-1"
          :args         {:path "/tmp/input.txt"}
          :abort-signal ::abort-signal}
         success-seen))
  (let [result-message (get-in success-signals [1 :tool-result-message])]
    (is (= result-message (get-in success-signals [1 :result])))
    (is (= :tool-result (:role result-message)))
    (is (= "tc-1" (:tool-call-id result-message)))
    (is (= "read_file" (:tool-name result-message)))
    (is (= [{:type :text :text "read /tmp/input.txt"}]
           (:content result-message)))
    (is (false? (:is-error? result-message)))
    (is (number? (:timestamp result-message))))

  (is (= [:llx.agent.signal/tool-error] (mapv :type missing-signals)))
  (is (= :llx/tool-not-found
         (-> missing-signals first :error ex-data :type)))
  (is (= "tc-1" (-> missing-signals first :tool-call-id)))
  (is (= "missing_tool" (-> missing-signals first :tool-result-message :tool-name)))
  (is (true? (-> missing-signals first :tool-result-message :is-error?)))
  (is (= (ex-message (-> missing-signals first :error))
         (get-in missing-signals [0 :tool-result-message :content 0 :text])))

  (is (= [:llx.agent.signal/tool-error] (mapv :type validation-signals)))
  (is (= :llx/validation-error
         (-> validation-signals first :error ex-data :type)))
  (is (false? validation-called?))
  (is (true? (-> validation-signals first :tool-result-message :is-error?)))

  (is (= [:llx.agent.signal/tool-error] (mapv :type throw-signals)))
  (is (= :test/tool-failed (-> throw-signals first :error ex-data :type)))
  (is (= "read failed"
         (get-in throw-signals [0 :tool-result-message :content 0 :text])))

  (is (= [:llx.agent.signal/tool-error] (mapv :type malformed-signals)))
  (is (= :llx/invalid-tool-result
         (-> malformed-signals first :error ex-data :type)))
  (is (true? (-> malformed-signals first :tool-result-message :is-error?)))

  (is (= [:llx.agent.signal/tool-result] (mapv :type stale-env-signals)))
  (is (= "from state"
         (get-in stale-env-signals [0 :tool-result-message :content 0 :text]))))

(deftest fx-call-llm-runs-hooks-and-maps-events-test
  #?(:clj
     (let [model                (ai/get-model :openai "gpt-4o")
           input-messages       [{:role :user :content "hello" :timestamp 1}]
           transformed-messages [{:role :user :content "trimmed" :timestamp 2}]
           llm-messages         [{:role :user :content "llm message" :timestamp 3}]
           final-message        (assistant-message model "Hello")
           seen*                (atom {})
           env                  (test-env
                                 (base-state model input-messages :medium [{:name "read_file"}])
                                 {:convert-to-llm     (fn [messages]
                                                        (swap! seen* assoc :convert-input messages)
                                                        (p/delay 1 llm-messages))
                                  :transform-context  (fn [messages signal]
                                                        (swap! seen* assoc :transform-input {:messages messages
                                                                                             :signal   signal})
                                                        (p/delay 1 transformed-messages))
                                  :stream-fn          (fn [m context options]
                                                        (swap! seen* assoc :stream-input {:model   m
                                                                                          :context context
                                                                                          :options options})
                                                        (stream-with-events
                                                         [{:type :start}
                                                          {:type :text-start}
                                                          {:type :text-delta :text "Hel"}
                                                          {:type :text-delta :text "lo"}
                                                          {:type :done :assistant-message final-message}]))
                                  :abort-signal       ::abort-signal
                                  :session-id         "session-1"
                                  :get-api-key        (fn [provider]
                                                        (swap! seen* assoc :get-api-key-provider provider)
                                                        (p/delay 1 "token"))
                                  :thinking-budgets   {:high 1234}
                                  :max-retry-delay-ms 2500})
           out                  (fx/execute-fx env {:llx.agent.fx/type :call-llm :messages input-messages})
           signals              (util/await! (read-signals* out) 3000 ::timeout)]
       (is (not (util/timeout-result? signals)))
       (is (= [:llx.agent.signal/llm-start
               :llx.agent.signal/llm-chunk
               :llx.agent.signal/llm-chunk
               :llx.agent.signal/llm-chunk
               :llx.agent.signal/llm-done]
              (mapv :type signals)))
       (is (= "" (get-in (nth signals 1) [:chunk :content 0 :text])))
       (is (= "Hel" (get-in (nth signals 2) [:chunk :content 0 :text])))
       (is (= "Hello" (get-in (nth signals 3) [:chunk :content 0 :text])))
       (is (= final-message (:message (last signals))))
       (is (= {:messages input-messages
               :signal   ::abort-signal}
              (:transform-input @seen*)))
       (is (= transformed-messages (:convert-input @seen*)))
       (is (= model (get-in @seen* [:stream-input :model])))
       (is (= {:system-prompt "sys"
               :messages      llm-messages
               :tools         [{:name "read_file"}]}
              (get-in @seen* [:stream-input :context])))
       (is (= :medium (get-in @seen* [:stream-input :options :reasoning])))
       (is (= ::abort-signal (get-in @seen* [:stream-input :options :signal])))
       (is (= "token" (get-in @seen* [:stream-input :options :api-key])))
       (is (= "openai" (:get-api-key-provider @seen*)))
       (is (= "session-1" (get-in @seen* [:stream-input :options :session-id])))
       (is (= {:high 1234} (get-in @seen* [:stream-input :options :thinking-budgets])))
       (is (= 2500 (get-in @seen* [:stream-input :options :max-retry-delay-ms])))
       (is (nil? (get-in @seen* [:stream-input :options :get-api-key]))))
     :cljs
     (util/async done
                 (let [model                (ai/get-model :openai "gpt-4o")
                       input-messages       [{:role :user :content "hello" :timestamp 1}]
                       transformed-messages [{:role :user :content "trimmed" :timestamp 2}]
                       llm-messages         [{:role :user :content "llm message" :timestamp 3}]
                       final-message        (assistant-message model "Hello")
                       seen*                (atom {})
                       env                  (test-env
                                             (base-state model input-messages :medium [{:name "read_file"}])
                                             {:convert-to-llm     (fn [messages]
                                                                    (swap! seen* assoc :convert-input messages)
                                                                    (p/delay 1 llm-messages))
                                              :transform-context  (fn [messages signal]
                                                                    (swap! seen* assoc :transform-input {:messages messages
                                                                                                         :signal   signal})
                                                                    (p/delay 1 transformed-messages))
                                              :stream-fn          (fn [m context options]
                                                                    (swap! seen* assoc :stream-input {:model   m
                                                                                                      :context context
                                                                                                      :options options})
                                                                    (stream-with-events
                                                                     [{:type :start}
                                                                      {:type :text-start}
                                                                      {:type :text-delta :text "Hel"}
                                                                      {:type :text-delta :text "lo"}
                                                                      {:type :done :assistant-message final-message}]))
                                              :abort-signal       ::abort-signal
                                              :session-id         "session-1"
                                              :get-api-key        (fn [provider]
                                                                    (swap! seen* assoc :get-api-key-provider provider)
                                                                    (p/delay 1 "token"))
                                              :thinking-budgets   {:high 1234}
                                              :max-retry-delay-ms 2500})
                       out                  (fx/execute-fx env {:llx.agent.fx/type :call-llm :messages input-messages})]
                   (-> (read-signals* out)
                       (p/then (fn [signals]
                                 (is (not (util/timeout-result? signals)))
                                 (is (= [:llx.agent.signal/llm-start
                                         :llx.agent.signal/llm-chunk
                                         :llx.agent.signal/llm-chunk
                                         :llx.agent.signal/llm-chunk
                                         :llx.agent.signal/llm-done]
                                        (mapv :type signals)))
                                 (is (= "" (get-in (nth signals 1) [:chunk :content 0 :text])))
                                 (is (= "Hel" (get-in (nth signals 2) [:chunk :content 0 :text])))
                                 (is (= "Hello" (get-in (nth signals 3) [:chunk :content 0 :text])))
                                 (is (= final-message (:message (last signals))))
                                 (is (= {:messages input-messages
                                         :signal   ::abort-signal}
                                        (:transform-input @seen*)))
                                 (is (= transformed-messages (:convert-input @seen*)))
                                 (is (= model (get-in @seen* [:stream-input :model])))
                                 (is (= {:system-prompt "sys"
                                         :messages      llm-messages
                                         :tools         [{:name "read_file"}]}
                                        (get-in @seen* [:stream-input :context])))
                                 (is (= :medium (get-in @seen* [:stream-input :options :reasoning])))
                                 (is (= ::abort-signal (get-in @seen* [:stream-input :options :signal])))
                                 (is (= "token" (get-in @seen* [:stream-input :options :api-key])))
                                 (is (= "openai" (:get-api-key-provider @seen*)))
                                 (is (= "session-1" (get-in @seen* [:stream-input :options :session-id])))
                                 (is (= {:high 1234} (get-in @seen* [:stream-input :options :thinking-budgets])))
                                 (is (= 2500 (get-in @seen* [:stream-input :options :max-retry-delay-ms])))
                                 (is (nil? (get-in @seen* [:stream-input :options :get-api-key])))))
                       (p/then (fn [_] (done)))
                       (p/catch (partial util/fail-and-done! done)))))))

(deftest fx-call-llm-uses-default-stream-fn-when-not-provided-test
  #?(:clj
     (let [model  (ai/get-model :openai "gpt-4o")
           seen*  (atom nil)
           events [{:type :start}
                   {:type :done :assistant-message (assistant-message model "ok")}]]
       (with-redefs [ai/default-env (fn [] ::ai-env)
                     ai/stream      (fn [runtime-env stream-model context options]
                                      (reset! seen* {:runtime-env runtime-env
                                                     :model       stream-model
                                                     :context     context
                                                     :options     options})
                                      (stream-with-events events))]
         (let [env     (test-env
                        (base-state model [{:role :user :content "x" :timestamp 1}] :low [])
                        {:convert-to-llm     identity
                         :abort-signal       ::abort-signal
                         :session-id         "session-default"
                         :get-api-key        (fn [_provider] "resolved-key")
                         :thinking-budgets   {:low 4321}
                         :max-retry-delay-ms 1500})
               out     (fx/execute-fx env {:llx.agent.fx/type :call-llm
                                           :messages          [{:role :user :content "hi" :timestamp 2}]})
               signals (util/await! (read-signals* out) 3000 ::timeout)]
           (is (not (util/timeout-result? signals)))
           (is (= [:llx.agent.signal/llm-start :llx.agent.signal/llm-done] (mapv :type signals)))
           (is (= ::ai-env (:runtime-env @seen*)))
           (is (= model (:model @seen*)))
           (is (= {:system-prompt "sys"
                   :messages      [{:role :user :content "hi" :timestamp 2}]
                   :tools         []}
                  (:context @seen*)))
           (is (= :low (get-in @seen* [:options :reasoning])))
           (is (= ::abort-signal (get-in @seen* [:options :signal])))
           (is (= "session-default" (get-in @seen* [:options :session-id])))
           (is (= "resolved-key" (get-in @seen* [:options :api-key])))
           (is (= {:low 4321} (get-in @seen* [:options :thinking-budgets])))
           (is (= 1500 (get-in @seen* [:options :max-retry-delay-ms])))
           (is (nil? (get-in @seen* [:options :get-api-key]))))))
     :cljs
     (util/async done
                 (let [model  (ai/get-model :openai "gpt-4o")
                       seen*  (atom nil)
                       events [{:type :start}
                               {:type :done :assistant-message (assistant-message model "ok")}]]
                   (with-redefs [ai/default-env (fn [] ::ai-env)
                                 ai/stream      (fn [runtime-env stream-model context options]
                                                  (reset! seen* {:runtime-env runtime-env
                                                                 :model       stream-model
                                                                 :context     context
                                                                 :options     options})
                                                  (stream-with-events events))]
                     (let [env (test-env
                                (base-state model [{:role :user :content "x" :timestamp 1}] :low [])
                                {:convert-to-llm     identity
                                 :abort-signal       ::abort-signal
                                 :session-id         "session-default"
                                 :get-api-key        (fn [_provider] "resolved-key")
                                 :thinking-budgets   {:low 4321}
                                 :max-retry-delay-ms 1500})
                           out (fx/execute-fx env {:llx.agent.fx/type :call-llm
                                                   :messages          [{:role :user :content "hi" :timestamp 2}]})]
                       (-> (read-signals* out)
                           (p/then (fn [signals]
                                     (is (not (util/timeout-result? signals)))
                                     (is (= [:llx.agent.signal/llm-start :llx.agent.signal/llm-done]
                                            (mapv :type signals)))
                                     (is (= ::ai-env (:runtime-env @seen*)))
                                     (is (= model (:model @seen*)))
                                     (is (= {:system-prompt "sys"
                                             :messages      [{:role :user :content "hi" :timestamp 2}]
                                             :tools         []}
                                            (:context @seen*)))
                                     (is (= :low (get-in @seen* [:options :reasoning])))
                                     (is (= ::abort-signal (get-in @seen* [:options :signal])))
                                     (is (= "session-default" (get-in @seen* [:options :session-id])))
                                     (is (= "resolved-key" (get-in @seen* [:options :api-key])))
                                     (is (= {:low 4321} (get-in @seen* [:options :thinking-budgets])))
                                     (is (= 1500 (get-in @seen* [:options :max-retry-delay-ms])))
                                     (is (nil? (get-in @seen* [:options :get-api-key])))))
                           (p/then (fn [_] (done)))
                           (p/catch (partial util/fail-and-done! done)))))))))

(deftest fx-call-llm-emits-llm-error-on-hook-failure-test
  #?(:clj
     (let [model   (ai/get-model :openai "gpt-4o")
           env     (test-env
                    (base-state model [] :off [])
                    {:convert-to-llm (fn [_] (throw (ex-info "boom" {:type ::hook-failed})))})
           out     (fx/execute-fx env {:llx.agent.fx/type :call-llm
                                       :messages          [{:role :user :content "hi" :timestamp 1}]})
           signals (util/await! (read-signals* out) 3000 ::timeout)]
       (is (not (util/timeout-result? signals)))
       (is (= 1 (count signals)))
       (is (= :llx.agent.signal/llm-error (:type (first signals))))
       (is (= ::hook-failed (-> signals first :error ex-data :type))))
     :cljs
     (util/async done
                 (let [model (ai/get-model :openai "gpt-4o")
                       env   (test-env
                              (base-state model [] :off [])
                              {:convert-to-llm (fn [_] (throw (ex-info "boom" {:type ::hook-failed})))})
                       out   (fx/execute-fx env {:llx.agent.fx/type :call-llm
                                                 :messages          [{:role :user :content "hi" :timestamp 1}]})]
                   (-> (read-signals* out)
                       (p/then (fn [signals]
                                 (is (not (util/timeout-result? signals)))
                                 (is (= 1 (count signals)))
                                 (is (= :llx.agent.signal/llm-error (:type (first signals))))
                                 (is (= ::hook-failed (-> signals first :error ex-data :type)))))
                       (p/then (fn [_] (done)))
                       (p/catch (partial util/fail-and-done! done)))))))

(deftest fx-call-llm-emits-llm-error-on-non-canonical-event-type-test
  #?(:clj
     (let [model   (ai/get-model :openai "gpt-4o")
           env     (test-env
                    (base-state model [] :off [])
                    {:stream-fn (fn [_model _context _options]
                                  (stream-with-events [{:type :start}
                                                       {:type "text_start"}]))})
           out     (fx/execute-fx env {:llx.agent.fx/type :call-llm
                                       :messages          [{:role :user :content "hi" :timestamp 1}]})
           signals (util/await! (read-signals* out) 3000 ::timeout)]
       (is (not (util/timeout-result? signals)))
       (is (= [:llx.agent.signal/llm-start :llx.agent.signal/llm-error]
              (mapv :type signals))))
     :cljs
     (util/async done
                 (let [model (ai/get-model :openai "gpt-4o")
                       env   (test-env
                              (base-state model [] :off [])
                              {:stream-fn (fn [_model _context _options]
                                            (stream-with-events [{:type :start}
                                                                 {:type "text_start"}]))})
                       out   (fx/execute-fx env {:llx.agent.fx/type :call-llm
                                                 :messages          [{:role :user :content "hi" :timestamp 1}]})]
                   (-> (read-signals* out)
                       (p/then (fn [signals]
                                 (is (not (util/timeout-result? signals)))
                                 (is (= [:llx.agent.signal/llm-start :llx.agent.signal/llm-error]
                                        (mapv :type signals)))))
                       (p/then (fn [_] (done)))
                       (p/catch (partial util/fail-and-done! done)))))))

(deftest execute-fx-validates-effect-shape-test
  #?(:clj
     (let [model (ai/get-model :openai "gpt-4o")
           env   (test-env (base-state model [] :off []) {})]
       (is (thrown? Exception
                    (fx/execute-fx env {:llx.agent.fx/type :call-llm})))
       (is (thrown? Exception
                    (fx/execute-fx env {:llx.agent.fx/type :unknown})))
       (is (thrown? Exception
                    (fx/execute-fx env
                                   {:llx.agent.fx/type :emit-event
                                    :event             {:type :llx.agent.event/message-update}})))
       (let [results (util/await! (execute-tool-behaviors* model) 15000 ::timeout)]
         (is (not (util/timeout-result? results)))
         (assert-execute-tool-behaviors! results)))
     :cljs
     (util/async done
                 (let [model (ai/get-model :openai "gpt-4o")
                       env   (test-env (base-state model [] :off []) {})]
                   (is (thrown? js/Error
                                (fx/execute-fx env {:llx.agent.fx/type :call-llm})))
                   (is (thrown? js/Error
                                (fx/execute-fx env {:llx.agent.fx/type :unknown})))
                   (is (thrown? js/Error
                                (fx/execute-fx env
                                               {:llx.agent.fx/type :emit-event
                                                :event             {:type :llx.agent.event/message-update}})))
                   (-> (execute-tool-behaviors* model)
                       (p/then (fn [results]
                                 (is (not (util/timeout-result? results)))
                                 (assert-execute-tool-behaviors! results)))
                       (p/then (fn [_] (done)))
                       (p/catch (partial util/fail-and-done! done)))))))
