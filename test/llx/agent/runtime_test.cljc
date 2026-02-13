(ns llx.agent.runtime-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]]
       :cljs [[cljs.test :refer-macros [deftest is]]])
   #?@(:clj [[llx.ai.test-util :as util]]
       :cljs [[llx.ai.test-util :as util :include-macros true]])
   [llx.agent.runtime :as sut]
   [promesa.core :as p]))

(def base-user-message
  {:role      :user
   :content   [{:type :text :text "hello"}]
   :timestamp 1730000000000})

(def base-assistant-message
  {:role        :assistant
   :content     [{:type :text :text "done"}]
   :api         :openai-completions
   :provider    :openai
   :model       "gpt-4o-mini"
   :usage       {:input        0
                 :output       0
                 :cache-read   0
                 :cache-write  0
                 :total-tokens 0
                 :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
   :stop-reason :stop
   :timestamp   1730000000001})

(defn- now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn- wait-until*
  [pred attempts]
  #?(:clj
     (if (loop [remaining attempts]
           (cond
             (pred) true
             (zero? remaining) false
             :else (do
                     (Thread/sleep 10)
                     (recur (dec remaining)))))
       (p/resolved true)
       (p/rejected (ex-info "Timed out waiting for runtime condition."
                            {:type :llx.agent/runtime-test-timeout})))
     :cljs
     (if (pred)
       (p/resolved true)
       (if (zero? attempts)
         (p/rejected (ex-info "Timed out waiting for runtime condition."
                              {:type :llx.agent/runtime-test-timeout}))
         (-> (p/delay 10 true)
             (p/then (fn [_]
                       (wait-until* pred (dec attempts)))))))))

(defn- pause*
  [ms]
  #?(:clj (do
            (Thread/sleep ms)
            (p/resolved true))
     :cljs (p/delay ms true)))

(defn- expect-rejection*
  [deferred assert-error!]
  (-> deferred
      (p/then (fn [_]
                (throw (ex-info "Expected rejection."
                                {:type :llx.agent/runtime-test-expected-rejection}))))
      (p/catch (fn [ex]
                 (assert-error! ex)
                 true))))

(defn- emit-input-messages!
  [emit-event! messages]
  (doseq [message messages]
    (emit-event! {:llx.agent.event/type    :message-start
                  :llx.agent.event/message message})
    (emit-event! {:llx.agent.event/type    :message-end
                  :llx.agent.event/message message})))

(defn- emit-assistant-message!
  [emit-event! assistant]
  (emit-event! {:llx.agent.event/type    :message-start
                :llx.agent.event/message assistant})
  (emit-event! {:llx.agent.event/type    :message-end
                :llx.agent.event/message assistant})
  (emit-event! {:llx.agent.event/type     :agent-end
                :llx.agent.event/messages [assistant]}))

(defn- deferred-handle
  []
  (let [resolve* (atom nil)
        reject*  (atom nil)
        deferred (p/create (fn [resolve reject]
                             (reset! resolve* resolve)
                             (reset! reject* reject)))]
    {:deferred deferred
     :resolve! (fn [value]
                 (when-let [resolve @resolve*]
                   (resolve value)))
     :reject!  (fn [error]
                 (when-let [reject @reject*]
                   (reject error)))}))

(defn- make-controllable-runner
  []
  (let [calls*   (atom [])
        counter* (atom 0)]
    {:calls* calls*
     :run!   (fn [input]
               (let [ticket     (deferred-handle)
                     cancelled* (atom false)
                     id         (swap! counter* inc)
                     call       {:id         id
                                 :input      input
                                 :cancelled* cancelled*
                                 :resolve!   (:resolve! ticket)
                                 :reject!    (:reject! ticket)}]
                 (emit-input-messages! (:emit-event! input) (:messages input))
                 (swap! calls* conj call)
                 {:result  (:deferred ticket)
                  :cancel! (fn []
                             (reset! cancelled* true)
                             ((:emit-event! input)
                              {:llx.agent.event/type     :agent-end
                               :llx.agent.event/messages []})
                             ((:resolve! ticket) {:status :aborted}))}))}))

(defn- make-immediate-runner
  [calls*]
  (fn [input]
    (emit-input-messages! (:emit-event! input) (:messages input))
    (emit-assistant-message! (:emit-event! input)
                             (assoc base-assistant-message :timestamp (now-ms)))
    (swap! calls* conj input)
    {:result  (p/resolved {:status :ok})
     :cancel! (fn [] nil)}))

(defn- make-never-settling-runner
  [calls*]
  (fn [input]
    (emit-input-messages! (:emit-event! input) (:messages input))
    (let [cancelled* (atom false)]
      (swap! calls* conj {:input input :cancelled* cancelled*})
      {:result  (p/create (fn [_ _] nil))
       :cancel! (fn []
                  (reset! cancelled* true))})))

(deftest prompt-and-continue-reject-while-active
  (util/async done
              (let [{:keys [calls* run!]} (make-controllable-runner)
                    runtime               (sut/create-runtime {:run-command! run!})
                    first-prompt          (sut/prompt! runtime base-user-message)]
                (-> (wait-until* #(= 1 (count @calls*)) 100)
                    (p/then (fn [_]
                              (is (= 1 (count @calls*)))
                              (-> (expect-rejection* (sut/prompt! runtime base-user-message)
                                                     (fn [ex]
                                                       (is (or (= :runtime-busy (-> ex ex-data :reason))
                                                               (= :llx.agent.demo-fsm/command-rejected (-> ex ex-data :type))))))
                                  (p/then (fn [_]
                                            (expect-rejection* (sut/continue! runtime)
                                                               (fn [ex]
                                                                 (is (or (= :runtime-busy (-> ex ex-data :reason))
                                                                         (= :llx.agent.demo-fsm/command-rejected (-> ex ex-data :type)))))))))))
                    (p/then (fn [_]
                              ((:resolve! (first @calls*)) {:status :ok})
                              first-prompt))
                    (p/then (fn [result]
                              (is (= {:status :ok} result))
                              (is (= [base-user-message]
                                     (:messages (sut/state runtime))))
                              (sut/wait-for-idle runtime)))
                    (p/then (fn [idle?]
                              (is (true? idle?))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest prompt-rejects-invalid-command-payload-before-runner
  (util/async done
              (let [calls*  (atom [])
                    runtime (sut/create-runtime {:run-command! (make-immediate-runner calls*)})]
                (-> (expect-rejection* (sut/prompt! runtime {:content   [{:type :text :text "bad"}]
                                                             :timestamp 1730000000010})
                                       (fn [ex]
                                         (is (re-find #"Schema validation failed|Validation Error"
                                                      (ex-message ex)))))
                    (p/then (fn [_]
                              (is (empty? @calls*))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest prompt-rejects-invalid-runner-result-shape
  (util/async done
              (let [runtime (sut/create-runtime
                             {:run-command! (fn [_]
                                              (p/resolved {:status :ok}))})]
                (-> (expect-rejection* (sut/prompt! runtime base-user-message)
                                       (fn [ex]
                                         (is (re-find #"Schema validation failed|Validation Error"
                                                      (ex-message ex)))))
                    (p/then (fn [_]
                              (let [state (sut/state runtime)]
                                (is (false? (:streaming? state))))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest prompt-rejects-invalid-runner-result-value
  (util/async done
              (let [runtime (sut/create-runtime
                             {:run-command! (fn [_]
                                              {:result  (p/resolved :ok)
                                               :cancel! (fn [] nil)})})]
                (-> (expect-rejection* (sut/prompt! runtime base-user-message)
                                       (fn [ex]
                                         (is (re-find #"Schema validation failed|Validation Error"
                                                      (ex-message ex)))))
                    (p/then (fn [_]
                              (let [state (sut/state runtime)]
                                (is (false? (:streaming? state))))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest create-runtime-validates-options
  (is (thrown-with-msg?
       #?(:clj clojure.lang.ExceptionInfo
          :cljs js/Error)
       #"Schema validation failed|Validation Error"
       (sut/create-runtime {:run-command! :not-a-fn}))))

(deftest continue-from-assistant-tail-prefers-steering-then-follow-up
  (util/async done
              (let [calls*            (atom [])
                    runtime           (sut/create-runtime {:initial-state {:messages [base-user-message base-assistant-message]}
                                                           :run-command!  (make-immediate-runner calls*)})
                    steering-message  {:role      :user
                                       :content   [{:type :text :text "steer now"}]
                                       :timestamp 1730000000010}
                    follow-up-message {:role      :user
                                       :content   [{:type :text :text "follow later"}]
                                       :timestamp 1730000000020}]
                (-> (sut/steer! runtime steering-message)
                    (p/then (fn [_]
                              (sut/follow-up! runtime follow-up-message)))
                    (p/then (fn [_]
                              (sut/continue! runtime)))
                    (p/then (fn [result]
                              (is (= {:status :ok} result))
                              (is (= [steering-message] (:messages (first @calls*))))
                              (sut/continue! runtime)))
                    (p/then (fn [result]
                              (is (= {:status :ok} result))
                              (is (= [follow-up-message] (:messages (second @calls*))))
                              (expect-rejection* (sut/continue! runtime)
                                                 (fn [ex]
                                                   (is (or (= :runtime-no-queued-messages (-> ex ex-data :reason))
                                                           (= :llx.agent.demo-fsm/command-rejected (-> ex ex-data :type))))))))
                    (p/then (fn [_]
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest continue-rejects-when-no-messages
  (util/async done
              (let [calls*  (atom [])
                    runtime (sut/create-runtime {:run-command! (make-immediate-runner calls*)})]
                (-> (expect-rejection* (sut/continue! runtime)
                                       (fn [ex]
                                         (is (= :runtime-no-messages (-> ex ex-data :reason)))))
                    (p/then (fn [_]
                              (is (empty? @calls*))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest runtime-forwards-turn-hooks-and-runtime-config-to-runner
  (util/async done
              (let [calls*            (atom [])
                    convert-to-llm    (fn [messages] messages)
                    transform-context (fn [messages _signal]
                                        (p/resolved messages))
                    get-api-key       (fn [_provider] "api-key")
                    stream-fn         (fn [_model _context _opts])
                    runtime           (sut/create-runtime
                                       {:run-command!       (fn [input]
                                                              (swap! calls* conj input)
                                                              {:result  (p/resolved {:status :ok})
                                                               :cancel! (fn [] nil)})
                                        :convert-to-llm     convert-to-llm
                                        :transform-context  transform-context
                                        :get-api-key        get-api-key
                                        :stream-fn          stream-fn
                                        :session-id         "session-123"
                                        :thinking-budgets   {:minimal 128 :low 512}
                                        :max-retry-delay-ms 60000
                                        :steering-mode      :all
                                        :follow-up-mode     :one-at-a-time})]
                (-> (sut/prompt! runtime base-user-message)
                    (p/then (fn [_]
                              (is (= 1 (count @calls*)))
                              (let [call (first @calls*)]
                                (is (= convert-to-llm (:convert-to-llm call)))
                                (is (= transform-context (:transform-context call)))
                                (is (= get-api-key (:get-api-key call)))
                                (is (= stream-fn (:stream-fn call)))
                                (is (= "session-123" (:session-id call)))
                                (is (= {:minimal 128 :low 512}
                                       (:thinking-budgets call)))
                                (is (= 60000 (:max-retry-delay-ms call))))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest abort-calls-turn-cancel-and-wait-for-idle
  (util/async done
              (let [{:keys [calls* run!]} (make-controllable-runner)
                    runtime               (sut/create-runtime {:run-command! run!})
                    prompt-result         (sut/prompt! runtime base-user-message)]
                (-> (wait-until* #(= 1 (count @calls*)) 100)
                    (p/then (fn [_]
                              (is (= 1 (count @calls*)))
                              (sut/abort! runtime)))
                    (p/then (fn [aborted?]
                              (is (true? aborted?))
                              (is (true? @(-> @calls* first :cancelled*)))
                              prompt-result))
                    (p/then (fn [result]
                              (is (= {:status :aborted} result))
                              (sut/wait-for-idle runtime)))
                    (p/then (fn [idle?]
                              (is (true? idle?))
                              (sut/abort! runtime)))
                    (p/then (fn [aborted?]
                              (is (false? aborted?))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest close-aborts-active-turn-and-rejects-pending-command
  (util/async done
              (let [calls*        (atom [])
                    runtime       (sut/create-runtime {:run-command! (make-never-settling-runner calls*)})
                    prompt-result (sut/prompt! runtime base-user-message)]
                (-> (wait-until* #(= 1 (count @calls*)) 100)
                    (p/then (fn [_]
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (is (true? @(-> @calls* first :cancelled*)))
                              (expect-rejection*
                               prompt-result
                               (fn [ex]
                                 (is (or (= :runtime-closed (-> ex ex-data :reason))
                                         (= :llx.agent.demo-fsm/active-rejected (-> ex ex-data :type))))
                                 (let [state (sut/state runtime)]
                                   (is (false? (:streaming? state))))))))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest wait-for-idle-immediate-when-not-running
  (util/async done
              (let [runtime (sut/create-runtime {:run-command! (make-immediate-runner (atom []))})]
                (-> (sut/wait-for-idle runtime)
                    (p/then (fn [idle?]
                              (is (true? idle?))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest reset-clears-transient-state-and-preserves-core-config
  (util/async done
              (let [calls*  (atom [])
                    runtime (sut/create-runtime
                             {:initial-state {:system-prompt "system"
                                              :model         {:id "gpt-4o-mini"}
                                              :tools         [{:name "search"}]
                                              :messages      [base-user-message]
                                              :error         "boom"}
                              :run-command!  (make-immediate-runner calls*)})]
                (-> (sut/steer! runtime {:role :user :content [{:type :text :text "s1"}] :timestamp 1})
                    (p/then (fn [_]
                              (sut/follow-up! runtime {:role :user :content [{:type :text :text "f1"}] :timestamp 2})))
                    (p/then (fn [_]
                              (sut/reset! runtime)))
                    (p/then (fn [_]
                              (let [state (sut/state runtime)]
                                (is (= "system" (:system-prompt state)))
                                (is (= {:id "gpt-4o-mini"} (:model state)))
                                (is (= [{:name "search"}] (:tools state)))
                                (is (= [] (:messages state)))
                                (is (= [] (:steering-queue state)))
                                (is (= [] (:follow-up-queue state)))
                                (is (false? (:streaming? state)))
                                (is (nil? (:stream-message state)))
                                (is (= #{} (:pending-tool-calls state)))
                                (is (nil? (:error state))))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest subscribe-receives-events-and-unsubscribe-stops-delivery
  (util/async done
              (let [events*       (atom [])
                    count-before* (atom nil)
                    run!          (fn [{:keys [emit-event!]}]
                                    (emit-event! {:llx.agent.event/type    :message-start
                                                  :llx.agent.event/message base-assistant-message})
                                    (emit-event! {:llx.agent.event/type    :message-end
                                                  :llx.agent.event/message base-assistant-message})
                                    (emit-event! {:llx.agent.event/type     :agent-end
                                                  :llx.agent.event/messages [base-assistant-message]})
                                    {:result  (p/resolved {:status :ok})
                                     :cancel! (fn [] nil)})
                    runtime       (sut/create-runtime {:run-command! run!})
                    stop-sub      (sut/subscribe runtime (fn [event]
                                                           (swap! events* conj event)))]
                (-> (sut/prompt! runtime base-user-message)
                    (p/then (fn [_]
                              (wait-until* #(>= (count @events*) 3) 100)))
                    (p/then (fn [_]
                              (reset! count-before* (count @events*))
                              (is (true? (stop-sub)))
                              (sut/prompt! runtime base-user-message)))
                    (p/then (fn [_]
                              (pause* 20)))
                    (p/then (fn [_]
                              (is (= @count-before* (count @events*)))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))
