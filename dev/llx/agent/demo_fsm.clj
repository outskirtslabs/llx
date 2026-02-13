(ns llx.agent.demo-fsm
  (:refer-clojure :exclude [reset!])
  (:require
   [llx.agent.demo :as demo]
   [llx.agent.fsm :as fsm]
   [llx.agent.fx :as fx]
   [promesa.core :as p]))

(set! *warn-on-reflection* true)

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn user-message
  [text]
  {:role      :user
   :content   [{:type :text :text text}]
   :timestamp (now-ms)})

(defn- normalize-messages
  [message-or-messages]
  (cond
    (nil? message-or-messages) []
    (vector? message-or-messages) message-or-messages
    :else [message-or-messages]))

(defn- summarize-step
  [label step]
  {:label              label
   :event              (:event step)
   :before             (:before step)
   :after              (:after step)
   :supported?         (:supported? step)
   :planned-next-state (:planned-next-state step)
   :effect-ops         (mapv :op (:effects step))
   :effects            (:effects step)})

(defn- run-step!
  [env label event-or-name payload]
  (let [step (if (some? payload)
               (fsm/send! env event-or-name payload)
               (fsm/send! env event-or-name))]
    (summarize-step label step)))

(defn demo-fsm-plan!
  "Runs a deterministic walkthrough of llx.agent.fsm and returns a report.

  This is the pure transition/effect-plan walkthrough, without effect execution."
  []
  (let [env    (fsm/new-env)
        _      (fsm/start! env)
        steps  [(run-step! env
                           :prompt-command
                           fsm/cmd-prompt
                           {:messages [(user-message "hello state machine")]})
                (run-step! env
                           :runner-started
                           fsm/runner-started
                           nil)
                (run-step! env
                           :steer-while-running
                           fsm/cmd-steer
                           {:messages [(user-message "be concise")]})
                (run-step! env
                           :abort-command
                           fsm/cmd-abort
                           nil)
                (run-step! env
                           :runner-failed
                           fsm/runner-failed
                           {:error-message "aborted by operator"})
                (run-step! env
                           :reset-command
                           fsm/cmd-reset
                           nil)
                (run-step! env
                           :shutdown-command
                           fsm/cmd-shutdown
                           nil)
                (run-step! env
                           :prompt-after-closed
                           fsm/cmd-prompt
                           {:messages [(user-message "should be rejected")]})]
        report {:chart           fsm/runtime-statechart
                :initial-phase   fsm/idle
                :final-phase     (fsm/phase env)
                :supported-final (fsm/supported-events (fsm/phase env))
                :steps           steps}]
    (tap> report)
    report))

(defn- ensure-valid-mode
  [mode]
  (if (#{:all :one-at-a-time} mode)
    mode
    (throw (ex-info "Invalid queue mode." {:mode mode}))))

(defn- default-agent-state
  [overrides]
  (merge {:system-prompt      "You are the llx.agent FSM+FX demo runtime."
          :model              demo/demo-model
          :thinking-level     :off
          :tools              demo/demo-agent-tools
          :messages           []
          :streaming?         false
          :stream-message     nil
          :pending-tool-calls #{}
          :error              nil}
         overrides))

(defn- event->error
  [event]
  (let [turn-msg (:llx.agent.event/message event)]
    (when (and (= :turn-end (:llx.agent.event/type event))
               (map? turn-msg)
               (string? (:error-message turn-msg)))
      (:error-message turn-msg))))

(defn- apply-event
  [agent-state event]
  (let [event-type (:llx.agent.event/type event)
        message    (:llx.agent.event/message event)]
    (cond->
     (case event-type
       :message-start (assoc agent-state :stream-message message)
       :message-update (assoc agent-state :stream-message message)
       :message-end (-> agent-state
                        (assoc :stream-message nil)
                        (update :messages conj message))
       :tool-execution-start (update agent-state :pending-tool-calls conj
                                     (:llx.agent.event/tool-call-id event))
       :tool-execution-end (update agent-state :pending-tool-calls disj
                                   (:llx.agent.event/tool-call-id event))
       agent-state)
      (event->error event) (assoc :error (event->error event)))))

(defn- effect-messages
  [effect]
  (let [messages (:messages effect)]
    (if (and (map? messages) (contains? messages :messages))
      (:messages messages)
      messages)))

(defn- command-messages
  [effect]
  (let [payload (:payload effect)]
    (if (and (map? payload) (contains? payload :messages))
      (:messages payload)
      payload)))

(defn- safe-resolve
  [resolve value]
  (when (fn? resolve)
    (resolve value))
  nil)

(defn- safe-reject
  [reject ex]
  (when (fn? reject)
    (reject ex))
  nil)

(defn- effect->ex
  [type default-message effect]
  (let [error-value (:error effect)]
    (cond
      (instance? Throwable error-value)
      error-value

      (map? error-value)
      (ex-info (or (:error-message error-value) default-message)
               (merge {:type type}
                      error-value))

      :else
      (ex-info default-message
               (merge {:type type}
                      (select-keys effect [:reason]))))))

(defn- pop-slot!
  [state* slot]
  (let [entry* (atom nil)]
    (swap! state*
           (fn [state]
             (clojure.core/reset! entry* (get state slot))
             (assoc state slot nil)))
    @entry*))

(defn- pop-idle-waiters!
  [state*]
  (let [waiters* (atom [])]
    (swap! state*
           (fn [state]
             (clojure.core/reset! waiters* (vec (:idle-waiters state)))
             (assoc state :idle-waiters [])))
    @waiters*))

(defn- dequeue-queued-messages!
  [state* queue-key mode]
  (let [dequeued* (atom [])]
    (swap! state*
           (fn [state]
             (let [queue                                    (vec (or (get state queue-key) []))
                   [dequeued next-queue]
                   (if (= :all mode)
                     [queue []]
                     (if (seq queue)
                       [[(first queue)] (vec (rest queue))]
                       [[] queue]))]
               (clojure.core/reset! dequeued* dequeued)
               (assoc state queue-key next-queue))))
    @dequeued*))

(defn- continue-payload!
  [state*]
  (let [{:keys [agent-state steering-mode follow-up-mode]}
        @state*
        messages                                           (:messages agent-state)]
    (when (= :assistant (:role (last messages)))
      (let [steering-dequeued
            (dequeue-queued-messages! state*
                                      :steering-queue
                                      steering-mode)]
        (if (seq steering-dequeued)
          {:messages steering-dequeued}
          (let [follow-up-dequeued
                (dequeue-queued-messages! state*
                                          :follow-up-queue
                                          follow-up-mode)]
            (when (seq follow-up-dequeued)
              {:messages follow-up-dequeued})))))))

(defn- notify-subscribers!
  [state* event]
  (let [subscribers (vals (:subscribers @state*))]
    (run! (fn [handler]
            (try
              (handler event)
              (catch Exception _
                nil)))
          subscribers))
  nil)

(defn create-demo-runtime
  "Creates an exploratory FSM+FX runtime wired to `demo/demo-run-command!`.

  This runtime is intentionally minimal but fully functional:
  - command calls are translated to FSM events
  - FSM effects are executed through `llx.agent.fx`
  - `:invoke-runner!` runs real provider turns
  - command deferreds are resolved/rejected by effect callbacks"
  ([] (create-demo-runtime {}))
  ([{:keys [initial-state run-command! steering-mode follow-up-mode]
     :or   {run-command!   demo/demo-run-command!
            steering-mode  :one-at-a-time
            follow-up-mode :one-at-a-time}}]
   (let [fsm-env (fsm/new-env)
         _       (fsm/start! fsm-env)
         state*  (atom {:agent-state        (default-agent-state initial-state)
                        :steering-queue     []
                        :follow-up-queue    []
                        :steering-mode      (ensure-valid-mode steering-mode)
                        :follow-up-mode     (ensure-valid-mode follow-up-mode)
                        :pending-command    nil
                        :active-command     nil
                        :idle-waiters       []
                        :runner-cancel!     nil
                        :closed?            false
                        :step-log           []
                        :event-log          []
                        :subscribers        {}
                        :next-subscriber-id 0})]
     (letfn [(emit-event!
               [event]
               (swap! state*
                      (fn [state]
                        (-> state
                            (update :event-log conj event)
                            (update :agent-state apply-event event))))
               (notify-subscribers! state* event)
               (p/resolved true))

             (snapshot
               []
               (let [{:keys [agent-state steering-queue follow-up-queue
                             steering-mode follow-up-mode]}
                     @state*]
                 (assoc agent-state
                        :steering-queue steering-queue
                        :follow-up-queue follow-up-queue
                        :steering-mode steering-mode
                        :follow-up-mode follow-up-mode)))

             (resolve-slot!
               [slot value]
               (when-let [entry (pop-slot! state* slot)]
                 (safe-resolve (:resolve entry) value))
               true)

             (reject-slot!
               [slot ex]
               (when-let [entry (pop-slot! state* slot)]
                 (safe-reject (:reject entry) ex))
               true)

             (resolve-idle-waiters!
               []
               (let [waiters (pop-idle-waiters! state*)]
                 (run! (fn [{:keys [resolve]}]
                         (safe-resolve resolve true))
                       waiters))
               true)

             (reject-idle-waiters!
               [ex]
               (let [waiters (pop-idle-waiters! state*)]
                 (run! (fn [{:keys [reject]}]
                         (safe-reject reject ex))
                       waiters))
               true)

             (dispatch!
               ([event]
                (dispatch! event nil))
               ([event payload]
                (-> (if (some? payload)
                      (fx/dispatch-event! (fx-context) fsm-env event payload)
                      (fx/dispatch-event! (fx-context) fsm-env event))
                    (p/then (fn [step-result]
                              (swap! state* update :step-log conj step-result)
                              step-result)))))

             (fx-context
               []
               {:start-turn!
                (fn [_ _]
                  (swap! state*
                         (fn [state]
                           (let [pending          (:pending-command state)
                                 next-agent-state (-> (:agent-state state)
                                                      (assoc :streaming? true
                                                             :stream-message nil
                                                             :pending-tool-calls #{}
                                                             :error nil))]
                             (-> state
                                 (assoc :active-command pending
                                        :pending-command nil
                                        :agent-state next-agent-state)))))
                  true)

                :finish-turn!
                (fn [_ effect]
                  (let [error-msg (or (:error-message effect)
                                      (get-in effect [:error :error-message]))]
                    (swap! state*
                           (fn [state]
                             (-> state
                                 (assoc :runner-cancel! nil)
                                 (update :agent-state
                                         (fn [agent-state]
                                           (cond-> (-> agent-state
                                                       (assoc :streaming? false
                                                              :stream-message nil
                                                              :pending-tool-calls #{}))
                                             (string? error-msg) (assoc :error error-msg))))))))
                  true)

                :invoke-runner!
                (fn [_ effect]
                  (let [command  (:command effect)
                        messages (normalize-messages (command-messages effect))]
                    (try
                      (let [{:keys [result cancel!]}
                            (run-command! {:command     command
                                           :messages    messages
                                           :state       (snapshot)
                                           :emit-event! emit-event!})]
                        (swap! state* assoc :runner-cancel! cancel!)
                        (-> (dispatch! fsm/runner-started)
                            (p/catch (fn [_] nil)))
                        (-> result
                            (p/then (fn [value]
                                      (swap! state* assoc :runner-cancel! nil)
                                      (dispatch! fsm/runner-succeeded value)))
                            (p/catch (fn [ex]
                                       (swap! state* assoc :runner-cancel! nil)
                                       (dispatch! fsm/runner-failed
                                                  {:error-message (or (ex-message ex) "Runner failed.")
                                                   :error         ex})))
                            (p/catch (fn [_] nil))))
                      (catch Exception ex
                        (swap! state* assoc :runner-cancel! nil)
                        (-> (dispatch! fsm/runner-start-failed
                                       {:error-message (or (ex-message ex) "Runner start failed.")
                                        :error         ex})
                            (p/catch (fn [_] nil)))))
                    {:status :started}))

                :cancel-runner!
                (fn [_ _]
                  (when-let [cancel! (:runner-cancel! @state*)]
                    (cancel!))
                  (swap! state* assoc :runner-cancel! nil)
                  true)

                :resolve-command!
                (fn [_ effect]
                  (resolve-slot! :pending-command (get effect :value true)))

                :reject-command!
                (fn [_ effect]
                  (reject-slot! :pending-command
                                (effect->ex :llx.agent.demo-fsm/command-rejected
                                            "Command rejected."
                                            effect)))

                :resolve-active!
                (fn [_ effect]
                  (resolve-slot! :active-command (get effect :value true)))

                :reject-active!
                (fn [_ effect]
                  (reject-slot! :active-command
                                (effect->ex :llx.agent.demo-fsm/active-rejected
                                            "Active turn failed."
                                            effect)))

                :resolve-waiters!
                (fn [_ _]
                  (resolve-idle-waiters!))

                :reject-waiters!
                (fn [_ effect]
                  (reject-idle-waiters! (effect->ex :llx.agent.demo-fsm/wait-rejected
                                                    "Idle wait rejected."
                                                    effect)))

                :enqueue-idle-waiter!
                (fn [_ _]
                  (swap! state*
                         (fn [state]
                           (if-let [pending (:pending-command state)]
                             (-> state
                                 (update :idle-waiters conj pending)
                                 (assoc :pending-command nil))
                             state)))
                  true)

                :enqueue-steering!
                (fn [_ effect]
                  (swap! state* update :steering-queue into (normalize-messages (effect-messages effect)))
                  true)

                :enqueue-follow-up!
                (fn [_ effect]
                  (swap! state* update :follow-up-queue into (normalize-messages (effect-messages effect)))
                  true)

                :close-runtime!
                (fn [_ _]
                  (swap! state* assoc :closed? true)
                  true)})

             (submit-command!
               [command payload]
               (p/create
                (fn [resolve reject]
                  (let [entry      {:command command
                                    :resolve resolve
                                    :reject  reject}
                        accepted?* (atom false)]
                    (swap! state*
                           (fn [state]
                             (if (nil? (:pending-command state))
                               (do
                                 (clojure.core/reset! accepted?* true)
                                 (assoc state :pending-command entry))
                               state)))
                    (if-not @accepted?*
                      (reject (ex-info "Command slot busy."
                                       {:type    :llx.agent.demo-fsm/command-slot-busy
                                        :command command}))
                      (-> (if (some? payload)
                            (fx/dispatch-command! (fx-context) fsm-env command payload)
                            (fx/dispatch-command! (fx-context) fsm-env command))
                          (p/then (fn [step-result]
                                    (swap! state* update :step-log conj step-result)
                                    nil))
                          (p/catch (fn [ex]
                                     (swap! state*
                                            (fn [state]
                                              (if (= entry (:pending-command state))
                                                (assoc state :pending-command nil)
                                                state)))
                                     (reject ex)))))))))

             (subscribe!
               [handler]
               (let [id            (swap! state*
                                          (fn [state]
                                            (let [next-id (inc (:next-subscriber-id state))]
                                              (-> state
                                                  (assoc :next-subscriber-id next-id)
                                                  (assoc-in [:subscribers next-id] handler)))))
                     subscriber-id (:next-subscriber-id id)]
                 (fn []
                   (swap! state* update :subscribers dissoc subscriber-id)
                   true)))]
       {:fsm-env         fsm-env
        :state*          state*
        :submit-command! submit-command!
        :subscribe!      subscribe!
        :dispatch-event! dispatch!
        :run-command!    run-command!}))))

(defn state
  [runtime]
  (let [{:keys [state* fsm-env]}                           runtime
        {:keys [agent-state steering-queue follow-up-queue
                closed? step-log event-log]}
        @state*]
    (assoc agent-state
           :phase           (fsm/phase fsm-env)
           :steering-queue  steering-queue
           :follow-up-queue follow-up-queue
           :closed?         closed?
           :step-count      (count step-log)
           :event-count     (count event-log))))

(defn prompt!
  [runtime message-or-messages]
  ((:submit-command! runtime)
   :prompt
   {:messages (normalize-messages message-or-messages)}))

(defn continue!
  [runtime]
  ((:submit-command! runtime)
   :continue
   (when (= fsm/idle (fsm/phase (:fsm-env runtime)))
     (continue-payload! (:state* runtime)))))

(defn steer!
  [runtime message-or-messages]
  ((:submit-command! runtime)
   :steer
   {:messages (normalize-messages message-or-messages)}))

(defn follow-up!
  [runtime message-or-messages]
  ((:submit-command! runtime)
   :follow-up
   {:messages (normalize-messages message-or-messages)}))

(defn abort!
  [runtime]
  ((:submit-command! runtime) :abort nil))

(defn wait-for-idle
  [runtime]
  ((:submit-command! runtime) :wait nil))

(defn reset!
  [runtime]
  ((:submit-command! runtime) :reset nil))

(defn close!
  [runtime]
  ((:submit-command! runtime) :shutdown nil))

(defn subscribe
  [runtime handler]
  ((:subscribe! runtime) handler))

(defn await!
  [deferred]
  (demo/await! deferred))

(defn demo-fsm!
  "Runs a full provider-backed turn flow via FSM+FX and returns a report."
  []
  (let [runtime         (create-demo-runtime)
        events*         (atom [])
        unsubscribe     (subscribe runtime #(swap! events* conj %))
        prompt-result   (await! (prompt! runtime (user-message "hello from FSM runtime")))
        _               (await! (steer! runtime (user-message "be concise")))
        _               (await! (follow-up! runtime (user-message "summarize in one paragraph")))
        continue-result (await! (continue! runtime))
        _               (await! (wait-for-idle runtime))
        final-state     (state runtime)
        _               (await! (close! runtime))
        report          {:prompt-result   prompt-result
                         :continue-result continue-result
                         :final-state     final-state
                         :events          @events*
                         :step-log        (:step-log @(:state* runtime))}]
    (unsubscribe)
    (tap> report)
    report))

(comment
  ;; Pure transition/effect planning demo:
  (demo-fsm-plan!)

  ;; Full provider-backed demo:
  ;;
  ;; Requires OPENAI_API_KEY in env because this uses `demo/demo-run-command!`.
  (def rt (create-demo-runtime))

  (def stop-events
    (subscribe
     rt
     (fn [event]
       (tap> event))))

  (def prompt-result
    (await! (prompt! rt (user-message "hello from FSM runtime"))))

  (def steer-result
    (await! (steer! rt (user-message "be concise"))))

  (def follow-up-result
    (await! (follow-up! rt (user-message "summarize in one paragraph"))))

  (def continue-result
    (await! (continue! rt)))

  (def wait-result
    (await! (wait-for-idle rt)))

  (def current-state
    (state rt))

  (def close-result
    (await! (close! rt)))

  prompt-result
  steer-result
  follow-up-result
  continue-result
  wait-result
  current-state
  close-result

  (stop-events)

  ;; One-shot full report:
  (demo-fsm!))
