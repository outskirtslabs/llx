(ns llx.agent.runtime
  "Statechart-backed runtime coordinator for llx.agent command execution.

  This runtime composes:
  - `llx.agent.fsm` for command/state transitions.
  - `llx.agent.fx` for effect execution.

  The mutable coordination state lives in one atom (`:runtime-state*`), while
  transition planning remains in the statechart.

  `:run-command!` must return `{:result deferred :cancel! fn}` and `:result`
  must resolve to `{:status keyword}`."
  (:refer-clojure :exclude [reset!])
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn >defn-]]
   [llx.agent.fsm :as fsm]
   [llx.agent.fx :as fx]
   [llx.agent.loop :as agent-loop]
   [llx.agent.schema :as agent-schema]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(def default-agent-state
  {:system-prompt      ""
   :model              nil
   :thinking-level     :off
   :tools              []
   :messages           []
   :streaming?         false
   :stream-message     nil
   :pending-tool-calls #{}
   :error              nil})

(def command->schema-command
  {:prompt    :llx.agent.command/prompt
   :continue  :llx.agent.command/continue
   :steer     :llx.agent.command/steer
   :follow-up :llx.agent.command/follow-up
   :abort     :llx.agent.command/abort
   :reset     :llx.agent.command/reset
   :wait      :llx.agent.command/wait
   :shutdown  :llx.agent.command/shutdown})

(defn- throwable?
  [x]
  #?(:clj (instance? Throwable x)
     :cljs (instance? js/Error x)))

(defn- now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(>defn- normalize-messages
        [message-or-messages]
        [:llx.agent/command-message-input => [:vector :llx.agent/message]]
        (cond
          (nil? message-or-messages) []
          (vector? message-or-messages) message-or-messages
          :else [message-or-messages]))

(>defn- ensure-valid-mode
        [mode]
        [:llx.agent/queue-mode => :llx.agent/queue-mode]
        mode)

(defn- command-rejected-error
  [reason]
  (ex-info
   "Command rejected."
   {:type   :llx.agent.runtime/command-rejected
    :reason reason}))

(defn- active-rejected-error
  [reason]
  (ex-info
   "Active command rejected."
   {:type   :llx.agent.runtime/active-rejected
    :reason reason}))

(defn- wait-rejected-error
  [reason]
  (ex-info
   "Idle waiter rejected."
   {:type   :llx.agent.runtime/wait-rejected
    :reason reason}))

(defn- no-queued-messages-error
  []
  (active-rejected-error :runtime-no-queued-messages))

(defn- no-messages-error
  []
  (active-rejected-error :runtime-no-messages))

(defn- command-slot-busy-error
  [command]
  (ex-info
   "Command slot busy."
   {:type    :llx.agent.runtime/command-slot-busy
    :command command}))

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

(defn- command-messages
  [effect]
  (let [payload (:payload effect)]
    (if (and (map? payload) (contains? payload :messages))
      (:messages payload)
      payload)))

(defn- effect-messages
  [effect]
  (let [messages (:messages effect)]
    (if (and (map? messages) (contains? messages :messages))
      (:messages messages)
      messages)))

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
  [default-ex effect]
  (let [error-value (:error effect)
        reason      (:reason effect)]
    (cond
      (throwable? error-value)
      error-value

      (map? error-value)
      (ex-info (or (:error-message error-value)
                   (ex-message default-ex))
               (merge (ex-data default-ex)
                      (select-keys effect [:reason])
                      error-value))

      :else
      (if reason
        (ex-info (ex-message default-ex)
                 (assoc (ex-data default-ex) :reason reason))
        default-ex))))

(defn- queue-dequeue
  [queue mode]
  (if (= :all mode)
    [queue []]
    (if (seq queue)
      [[(first queue)] (vec (rest queue))]
      [[] queue])))

(defn- dequeue-runtime-queue!
  [runtime-state* queue-key mode-key]
  (let [messages* (volatile! [])]
    (swap! runtime-state*
           (fn [runtime-state]
             (let [[messages next-queue]
                   (queue-dequeue (vec (get runtime-state queue-key []))
                                  (get runtime-state mode-key))]
               (vreset! messages* messages)
               (assoc runtime-state queue-key next-queue))))
    @messages*))

(defn- pop-slot!
  [runtime-state* slot]
  (let [entry* (volatile! nil)]
    (swap! runtime-state*
           (fn [runtime-state]
             (vreset! entry* (get runtime-state slot))
             (assoc runtime-state slot nil)))
    @entry*))

(defn- pop-idle-waiters!
  [runtime-state*]
  (let [waiters* (volatile! [])]
    (swap! runtime-state*
           (fn [runtime-state]
             (vreset! waiters* (vec (:idle-waiters runtime-state)))
             (assoc runtime-state :idle-waiters [])))
    @waiters*))

(defn- allocate-run-token!
  [runtime-state*]
  (let [token* (volatile! nil)]
    (swap! runtime-state*
           (fn [runtime-state]
             (let [token (inc (or (:next-run-token runtime-state) 0))]
               (vreset! token* token)
               (assoc runtime-state
                      :next-run-token token
                      :active-run-token token))))
    @token*))

(defn- reset-runtime-transients
  [runtime-state]
  (let [agent-state           (:agent-state runtime-state)
        preserved-agent-state (select-keys agent-state
                                           [:system-prompt
                                            :model
                                            :thinking-level
                                            :tools])]
    (-> runtime-state
        (assoc :agent-state (merge default-agent-state
                                   preserved-agent-state)
               :steering-queue []
               :follow-up-queue []
               :idle-waiters []
               :runner-cancel! nil
               :active-run-token nil
               :active-command nil))))

(defn- state-snapshot
  [{:keys [agent-state steering-queue follow-up-queue
           steering-mode follow-up-mode closed?]}]
  (assoc agent-state
         :steering-queue steering-queue
         :follow-up-queue follow-up-queue
         :steering-mode steering-mode
         :follow-up-mode follow-up-mode
         :closed? closed?))

(defn- continue-input!
  [runtime-state*]
  (let [choice* (volatile! nil)]
    (swap! runtime-state*
           (fn [runtime-state]
             (let [agent-messages (get-in runtime-state [:agent-state :messages])]
               (if (seq agent-messages)
                 (if (= :assistant (:role (last agent-messages)))
                   (let [[steering steering-next] (queue-dequeue (:steering-queue runtime-state)
                                                                 (:steering-mode runtime-state))]
                     (if (seq steering)
                       (do
                         (vreset! choice*
                                  {:messages                    steering
                                   :skip-initial-steering-poll? true})
                         (assoc runtime-state :steering-queue steering-next))
                       (let [[follow-up follow-up-next]
                             (queue-dequeue (:follow-up-queue runtime-state)
                                            (:follow-up-mode runtime-state))]
                         (if (seq follow-up)
                           (do
                             (vreset! choice* {:messages follow-up})
                             (assoc runtime-state :follow-up-queue follow-up-next))
                           (do
                             (vreset! choice* {:error (no-queued-messages-error)})
                             runtime-state)))))
                   (do
                     (vreset! choice* {})
                     runtime-state))
                 (do
                   (vreset! choice* {:error (no-messages-error)})
                   runtime-state)))))
    @choice*))

(defn- string-prompt-message
  [text images]
  {:role      :user
   :content   (into [{:type :text :text text}] images)
   :timestamp (now-ms)})

(defn- prompt-input->messages
  [input images]
  (cond
    (string? input) (vector (string-prompt-message input (or images [])))
    (nil? images) (normalize-messages input)
    :else (throw (ex-info "Image attachments are only supported for string prompt input."
                          {:type :llx.agent.runtime/invalid-prompt-input}))))

(defn- resolve-command-input!
  [runtime-state* command effect]
  (let [payload-messages (command-messages effect)]
    (cond
      (some? payload-messages)
      {:messages                    (normalize-messages payload-messages)
       :skip-initial-steering-poll? (boolean (get-in effect [:payload :skip-initial-steering-poll?]))}

      (= :continue command)
      (continue-input! runtime-state*)

      :else
      {:messages nil})))

(defn- notify-subscribers!
  [runtime-state* event]
  (let [subscribers (vals (:subscribers @runtime-state*))]
    (run! (fn [handler]
            (try
              (handler event)
              (catch #?(:clj Exception :cljs :default) _
                nil)))
          subscribers))
  nil)

(defn- validate-command!
  [command payload]
  (let [schema-command (get command->schema-command command)]
    (when-not schema-command
      (throw (ex-info "Unknown runtime command."
                      {:type    :llx.agent.runtime/unknown-command
                       :command command})))
    (agent-schema/validate!
     :llx.agent/command
     (cond-> {:llx.agent.command/type schema-command}
       (contains? payload :messages) (assoc :messages (:messages payload))))))

(defn- runtime-handle
  [runtime]
  (agent-schema/validate! :llx.agent/runtime-handle runtime))

(defn- default-run-status
  [agent-end-event cancelled?]
  (let [messages       (or (:llx.agent.event/messages agent-end-event) [])
        last-assistant (last (filter #(= :assistant (:role %)) messages))]
    (cond
      cancelled? {:status :aborted}
      (nil? agent-end-event) {:status :error}
      (and (map? last-assistant)
           (contains? #{:error :aborted} (:stop-reason last-assistant)))
      {:status :error}
      :else
      {:status :ok})))

(defn- default-run-command!
  [runtime-state*]
  (fn [{:keys [command messages state emit-event!
               env model convert-to-llm transform-context stream-fn
               get-api-key session-id thinking-budgets max-retry-delay-ms
               skip-initial-steering-poll?]}]
    (try
      (let [cancelled?*  (atom false)
            skip-first?* (atom (boolean skip-initial-steering-poll?))
            loop-context {:system-prompt (:system-prompt state)
                          :messages      (vec (:messages state))
                          :tools         (vec (:tools state))}
            loop-config  (cond-> {:env                    env
                                  :model                  model
                                  :convert-to-llm         (or convert-to-llm identity)
                                  :transform-context      transform-context
                                  :stream-fn              stream-fn
                                  :get-api-key            get-api-key
                                  :session-id             session-id
                                  :thinking-budgets       thinking-budgets
                                  :max-retry-delay-ms     max-retry-delay-ms
                                  :get-follow-up-messages (fn []
                                                            (dequeue-runtime-queue! runtime-state*
                                                                                    :follow-up-queue
                                                                                    :follow-up-mode))
                                  :get-steering-messages  (fn []
                                                            (if (compare-and-set! skip-first?* true false)
                                                              []
                                                              (dequeue-runtime-queue! runtime-state*
                                                                                      :steering-queue
                                                                                      :steering-mode)))}
                           (nil? transform-context) (dissoc :transform-context)
                           (nil? stream-fn) (dissoc :stream-fn)
                           (nil? get-api-key) (dissoc :get-api-key)
                           (nil? session-id) (dissoc :session-id)
                           (nil? thinking-budgets) (dissoc :thinking-budgets)
                           (nil? max-retry-delay-ms) (dissoc :max-retry-delay-ms))
            events-ch    (if (= :continue command)
                           (agent-loop/agent-loop-continue loop-context loop-config)
                           (agent-loop/agent-loop (or messages []) loop-context loop-config))
            result       (letfn [(pump [agent-end-event]
                                   (p/let [event (sp/take events-ch)]
                                     (if (nil? event)
                                       (default-run-status agent-end-event @cancelled?*)
                                       (-> (emit-event! event)
                                           (p/then (fn [_]
                                                     (pump (if (= :agent-end (:llx.agent.event/type event))
                                                             event
                                                             agent-end-event))))))))]
                           (pump nil))]
        {:result  result
         :cancel! (fn []
                    (clojure.core/reset! cancelled?* true)
                    (sp/close events-ch)
                    nil)})
      (catch #?(:clj Exception :cljs :default) ex
        {:result  (p/rejected ex)
         :cancel! (fn [] nil)}))))

(defn- runtime-snapshot
  [runtime-state*]
  (state-snapshot @runtime-state*))

(defn- resolve-slot!
  [runtime-state* slot value]
  (when-let [entry (pop-slot! runtime-state* slot)]
    (safe-resolve (:resolve entry) value))
  true)

(defn- reject-slot!
  [runtime-state* slot ex]
  (when-let [entry (pop-slot! runtime-state* slot)]
    (safe-reject (:reject entry) ex))
  true)

(defn- resolve-idle-waiters!
  [runtime-state*]
  (let [waiters (pop-idle-waiters! runtime-state*)]
    (run! (fn [{:keys [resolve]}]
            (safe-resolve resolve true))
          waiters))
  true)

(defn- reject-idle-waiters!
  [runtime-state* ex]
  (let [waiters (pop-idle-waiters! runtime-state*)]
    (run! (fn [{:keys [reject]}]
            (safe-reject reject ex))
          waiters))
  true)

(defn- emit-runtime-event!
  [runtime-state* run-token event]
  (let [accepted?* (volatile! false)]
    (swap! runtime-state*
           (fn [runtime-state]
             (if (and (= run-token (:active-run-token runtime-state))
                      (not (:closed? runtime-state)))
               (do
                 (vreset! accepted?* true)
                 (-> runtime-state
                     (update :event-log conj event)
                     (update :agent-state apply-event event)))
               runtime-state)))
    (when @accepted?*
      (notify-subscribers! runtime-state* event))
    (p/resolved @accepted?*)))

(defn- dispatch-event!
  [runtime-state* fsm-env fx-context-fn event payload]
  (-> (if (some? payload)
        (fx/dispatch-event! (fx-context-fn) fsm-env event payload)
        (fx/dispatch-event! (fx-context-fn) fsm-env event))
      (p/then (fn [step-result]
                (swap! runtime-state* update :step-log conj step-result)
                step-result))))

(defn- create-fx-context
  [{:keys [runtime-state* run-command! dispatch!]}]
  {:start-turn!
   (fn [_ _]
     (swap! runtime-state*
            (fn [runtime-state]
              (let [pending          (:pending-command runtime-state)
                    next-agent-state (-> (:agent-state runtime-state)
                                         (assoc :streaming? true
                                                :stream-message nil
                                                :pending-tool-calls #{}
                                                :error nil))]
                (-> runtime-state
                    (assoc :active-command pending
                           :pending-command nil
                           :runner-cancel! nil
                           :active-run-token nil
                           :agent-state next-agent-state)))))
     true)

   :finish-turn!
   (fn [_ effect]
     (let [error-msg (or (:error-message effect)
                         (get-in effect [:error :error-message]))]
       (swap! runtime-state*
              (fn [runtime-state]
                (-> runtime-state
                    (assoc :runner-cancel! nil
                           :active-run-token nil)
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
     (let [command                                                (:command effect)
           {:keys [messages error skip-initial-steering-poll?]}
           (resolve-command-input! runtime-state* command effect)]
       (if error
         (do
           (-> (dispatch! fsm/runner-start-failed
                          {:error-message (ex-message error)
                           :error         error
                           :reason        (-> error ex-data :reason)})
               (p/catch (fn [_] nil)))
           {:status :start-failed})
         (try
           (let [run-token                       (allocate-run-token! runtime-state*)
                 emit-event                      (fn [event]
                                                   (emit-runtime-event! runtime-state* run-token event))
                 runtime-state                   @runtime-state*
                 runner-input                    {:command                     command
                                                  :messages                    (when (seq messages) messages)
                                                  :skip-initial-steering-poll? (boolean skip-initial-steering-poll?)
                                                  :state                       (runtime-snapshot runtime-state*)
                                                  :emit-event!                 emit-event
                                                  :env                         (:env runtime-state)
                                                  :model                       (get-in runtime-state [:agent-state :model])
                                                  :convert-to-llm              (:convert-to-llm runtime-state)
                                                  :transform-context           (:transform-context runtime-state)
                                                  :stream-fn                   (:stream-fn runtime-state)
                                                  :get-api-key                 (:get-api-key runtime-state)
                                                  :session-id                  (:session-id runtime-state)
                                                  :thinking-budgets            (:thinking-budgets runtime-state)
                                                  :max-retry-delay-ms          (:max-retry-delay-ms runtime-state)}
                 {:keys [result cancel!]}
                 (agent-schema/validate!
                  :llx.agent/runtime-turn-result
                  (run-command! runner-input))]
             (swap! runtime-state* assoc :runner-cancel! cancel!)
             (-> (dispatch! fsm/runner-started)
                 (p/catch (fn [_] nil)))
             (-> result
                 (p/then (fn [value]
                           (let [value (agent-schema/validate! :llx.agent/runtime-turn-value value)]
                             (dispatch! fsm/runner-succeeded value))))
                 (p/catch (fn [ex]
                            (dispatch! fsm/runner-failed
                                       {:error-message (or (ex-message ex)
                                                           "Runner failed.")
                                        :error         ex})))
                 (p/catch (fn [_] nil)))
             {:status :started})
           (catch #?(:clj Exception :cljs :default) ex
             (-> (dispatch! fsm/runner-start-failed
                            {:error-message (or (ex-message ex)
                                                "Runner start failed.")
                             :error         ex})
                 (p/catch (fn [_] nil)))
             {:status :start-failed})))))

   :cancel-runner!
   (fn [_ _]
     (when-let [cancel! (:runner-cancel! @runtime-state*)]
       (cancel!))
     (swap! runtime-state* assoc
            :runner-cancel! nil
            :active-run-token nil)
     true)

   :resolve-command!
   (fn [_ effect]
     (if (= :reset-snapshot (:value effect))
       (do
         (swap! runtime-state* reset-runtime-transients)
         (resolve-slot! runtime-state* :pending-command (runtime-snapshot runtime-state*)))
       (resolve-slot! runtime-state*
                      :pending-command
                      (get effect :value true))))

   :reject-command!
   (fn [_ effect]
     (reject-slot! runtime-state*
                   :pending-command
                   (effect->ex (command-rejected-error :runtime-rejected)
                               effect)))

   :resolve-active!
   (fn [_ effect]
     (resolve-slot! runtime-state*
                    :active-command
                    (get effect :value true)))

   :reject-active!
   (fn [_ effect]
     (let [error-msg (or (:error-message effect)
                         (get-in effect [:error :error-message]))]
       (swap! runtime-state*
              (fn [runtime-state]
                (-> runtime-state
                    (assoc :runner-cancel! nil
                           :active-run-token nil)
                    (update :agent-state
                            (fn [agent-state]
                              (cond-> (-> agent-state
                                          (assoc :streaming? false
                                                 :stream-message nil
                                                 :pending-tool-calls #{}))
                                (string? error-msg) (assoc :error error-msg))))))))
     (reject-slot! runtime-state*
                   :active-command
                   (effect->ex (active-rejected-error :runtime-failed)
                               effect)))

   :resolve-waiters!
   (fn [_ _]
     (resolve-idle-waiters! runtime-state*))

   :reject-waiters!
   (fn [_ effect]
     (reject-idle-waiters! runtime-state*
                           (effect->ex (wait-rejected-error :runtime-rejected)
                                       effect)))

   :enqueue-idle-waiter!
   (fn [_ _]
     (swap! runtime-state*
            (fn [runtime-state]
              (if-let [pending (:pending-command runtime-state)]
                (-> runtime-state
                    (update :idle-waiters conj pending)
                    (assoc :pending-command nil))
                runtime-state)))
     true)

   :enqueue-steering!
   (fn [_ effect]
     (swap! runtime-state* update :steering-queue into
            (normalize-messages (effect-messages effect)))
     true)

   :enqueue-follow-up!
   (fn [_ effect]
     (swap! runtime-state* update :follow-up-queue into
            (normalize-messages (effect-messages effect)))
     true)

   :close-runtime!
   (fn [_ _]
     (swap! runtime-state* assoc :closed? true)
     true)})

(defn- create-submit-command!
  [{:keys [runtime-state* fsm-env fx-context]}]
  (fn [command payload]
    (p/create
     (fn [resolve reject]
       (try
         (validate-command! command payload)
         (let [entry      {:command command
                           :resolve resolve
                           :reject  reject}
               accepted?* (volatile! false)]
           (swap! runtime-state*
                  (fn [runtime-state]
                    (if (nil? (:pending-command runtime-state))
                      (do
                        (vreset! accepted?* true)
                        (assoc runtime-state :pending-command entry))
                      runtime-state)))
           (if-not @accepted?*
             (reject (command-slot-busy-error command))
             (-> (if (seq payload)
                   (fx/dispatch-command! (fx-context) fsm-env command payload)
                   (fx/dispatch-command! (fx-context) fsm-env command))
                 (p/then (fn [step-result]
                           (swap! runtime-state* update :step-log conj step-result)
                           nil))
                 (p/catch (fn [ex]
                            (swap! runtime-state*
                                   (fn [runtime-state]
                                     (if (= entry (:pending-command runtime-state))
                                       (assoc runtime-state :pending-command nil)
                                       runtime-state)))
                            (reject ex))))))
         (catch #?(:clj Exception :cljs :default) ex
           (reject ex)))))))

(defn- create-subscribe!
  [runtime-state*]
  (fn [handler]
    (let [subscriber-id* (volatile! nil)]
      (swap! runtime-state*
             (fn [runtime-state]
               (let [subscriber-id (inc (:next-subscriber-id runtime-state))]
                 (vreset! subscriber-id* subscriber-id)
                 (-> runtime-state
                     (assoc :next-subscriber-id subscriber-id)
                     (assoc-in [:subscribers subscriber-id] handler)))))
      (fn []
        (let [removed?* (volatile! false)
              id        @subscriber-id*]
          (swap! runtime-state*
                 (fn [runtime-state]
                   (if (contains? (:subscribers runtime-state) id)
                     (do
                       (vreset! removed?* true)
                       (update runtime-state :subscribers dissoc id))
                     runtime-state)))
          @removed?*)))))

(defn- rejected-deferred
  [ex]
  (p/rejected ex))

(>defn create-runtime
       "Creates an FSM/FX-backed runtime coordinator.

  Options:

  - `:run-command!` (required): Turn runner function.
  - `:initial-state` (optional): Initial agent state overlay.
  - `:convert-to-llm` (optional): Conversion hook forwarded to runner.
  - `:transform-context` (optional): Context transform hook forwarded to runner.
  - `:stream-fn` (optional): Stream override forwarded to runner.
  - `:get-api-key` (optional): Dynamic key hook forwarded to runner.
  - `:session-id` (optional): Provider session id forwarded to runner.
  - `:thinking-budgets` (optional): Reasoning budgets forwarded to runner.
  - `:max-retry-delay-ms` (optional): Retry cap forwarded to runner.
  - `:steering-mode` (optional): `:one-at-a-time` or `:all`.
  - `:follow-up-mode` (optional): `:one-at-a-time` or `:all`."
       [opts]
       [:map => :llx.agent/runtime-handle]
       (let [{:keys [run-command! env model initial-state steering-mode follow-up-mode
                     convert-to-llm transform-context stream-fn get-api-key
                     session-id thinking-budgets max-retry-delay-ms]}
             (agent-schema/validate! :llx.agent/runtime-options opts)
             initial-state
             (cond-> (or initial-state {})
               (some? model) (assoc :model model))
             fsm-env
             (-> (fsm/new-env)
                 (fsm/start!))
             runtime-state*
             (atom {:agent-state        (merge default-agent-state
                                               initial-state)
                    :env                env
                    :convert-to-llm     convert-to-llm
                    :transform-context  transform-context
                    :stream-fn          stream-fn
                    :get-api-key        get-api-key
                    :session-id         session-id
                    :thinking-budgets   thinking-budgets
                    :max-retry-delay-ms max-retry-delay-ms
                    :steering-queue     []
                    :follow-up-queue    []
                    :steering-mode      (ensure-valid-mode (or steering-mode :one-at-a-time))
                    :follow-up-mode     (ensure-valid-mode (or follow-up-mode :one-at-a-time))
                    :pending-command    nil
                    :active-command     nil
                    :idle-waiters       []
                    :runner-cancel!     nil
                    :closed?            false
                    :step-log           []
                    :event-log          []
                    :subscribers        {}
                    :next-subscriber-id 0
                    :next-run-token     0
                    :active-run-token   nil})
             run-command!                                                                      (or run-command!
                                                                                                   (default-run-command! runtime-state*))]
         (letfn [(fx-context []
                   (create-fx-context {:runtime-state* runtime-state*
                                       :run-command!   run-command!
                                       :dispatch!      dispatch!}))

                 (dispatch!
                   ([event]
                    (dispatch! event nil))
                   ([event payload]
                    (dispatch-event! runtime-state* fsm-env fx-context event payload)))]
           {:fsm-env         fsm-env
            :runtime-state*  runtime-state*
            :submit-command! (create-submit-command! {:runtime-state* runtime-state*
                                                      :fsm-env        fsm-env
                                                      :fx-context     fx-context})
            :subscribe!      (create-subscribe! runtime-state*)
            :dispatch-event! dispatch!
            :run-command!    run-command!})))

(>defn state
       "Returns a snapshot of runtime-visible state.

  Includes queue data and current FSM phase for operational visibility."
       [runtime]
       [:map => :llx.agent/runtime-state-snapshot]
       (let [runtime                                                  (runtime-handle runtime)
             {:keys [runtime-state* fsm-env]}                         runtime
             {:keys [agent-state steering-queue follow-up-queue
                     steering-mode follow-up-mode closed?
                     session-id thinking-budgets max-retry-delay-ms]}
             @runtime-state*]
         (assoc agent-state
                :phase           (fsm/phase fsm-env)
                :steering-queue  steering-queue
                :follow-up-queue follow-up-queue
                :steering-mode   steering-mode
                :follow-up-mode  follow-up-mode
                :session-id      session-id
                :thinking-budgets thinking-budgets
                :max-retry-delay-ms max-retry-delay-ms
                :closed?         closed?)))

(>defn prompt!
       "Queues a prompt command.

  `message-or-messages` may be one message or a vector of messages."
       ([runtime message-or-messages]
        [:map :any => :llx/deferred]
        (try
          (let [runtime  (runtime-handle runtime)
                messages (prompt-input->messages message-or-messages nil)]
            ((:submit-command! runtime)
             :prompt
             {:messages messages}))
          (catch #?(:clj Exception :cljs :default) ex
            (rejected-deferred ex))))
       ([runtime text images]
        [:map :string [:vector :llx/content-image] => :llx/deferred]
        (try
          (let [runtime  (runtime-handle runtime)
                messages (prompt-input->messages text images)]
            ((:submit-command! runtime)
             :prompt
             {:messages messages}))
          (catch #?(:clj Exception :cljs :default) ex
            (rejected-deferred ex)))))

(>defn continue!
       "Queues a continue command.

  If the current transcript ends in assistant output, queued steering messages
  are consumed first, then follow-up queue messages."
       [runtime]
       [:map => :llx/deferred]
       (let [runtime (runtime-handle runtime)]
         ((:submit-command! runtime)
          :continue
          nil)))

(>defn steer!
       "Enqueues steering messages for the next eligible continue command."
       [runtime message-or-messages]
       [:map :any => :llx/deferred]
       (let [runtime (runtime-handle runtime)]
         ((:submit-command! runtime)
          :steer
          {:messages message-or-messages})))

(>defn follow-up!
       "Enqueues follow-up messages for the next eligible continue command."
       [runtime message-or-messages]
       [:map :any => :llx/deferred]
       (let [runtime (runtime-handle runtime)]
         ((:submit-command! runtime)
          :follow-up
          {:messages message-or-messages})))

(>defn abort!
       "Requests cancellation of the active turn.

  Resolves to `true` when a turn was active, else `false`."
       [runtime]
       [:map => :llx/deferred]
       (let [runtime (runtime-handle runtime)]
         ((:submit-command! runtime)
          :abort
          nil)))

(>defn wait-for-idle
       "Resolves when the runtime reaches idle.

  Returns immediately with `true` when already idle."
       [runtime]
       [:map => :llx/deferred]
       (let [runtime (runtime-handle runtime)]
         ((:submit-command! runtime)
          :wait
          nil)))

(>defn reset!
       "Resets transient runtime state while preserving core configuration.

  Preserved keys:
  - `:system-prompt`
  - `:model`
  - `:thinking-level`
  - `:tools`"
       [runtime]
       [:map => :llx/deferred]
       (let [runtime (runtime-handle runtime)]
         ((:submit-command! runtime)
          :reset
          nil)))

(>defn close!
       "Shuts down the runtime and rejects active/pending waits as needed."
       [runtime]
       [:map => :llx/deferred]
       (let [runtime (runtime-handle runtime)]
         ((:submit-command! runtime)
          :shutdown
          nil)))

(>defn subscribe
       "Subscribes to emitted runtime events.

  Returns an unsubscribe function."
       [runtime handler]
       [:map [:fn fn?] => [:fn fn?]]
       (let [runtime (runtime-handle runtime)]
         ((:subscribe! runtime) handler)))

(>defn set-system-prompt!
       "Sets the runtime system prompt."
       [runtime system-prompt]
       [:map :string => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc-in [:agent-state :system-prompt] system-prompt)
         (state runtime)))

(>defn set-model!
       "Sets the active model in runtime state."
       [runtime model]
       [:map :llx/model => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc-in [:agent-state :model] model)
         (state runtime)))

(>defn set-thinking-level!
       "Sets the active thinking level."
       [runtime thinking-level]
       [:map :llx.agent/thinking-level => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc-in [:agent-state :thinking-level] thinking-level)
         (state runtime)))

(>defn set-tools!
       "Replaces the runtime tools vector."
       [runtime tools]
       [:map [:vector :llx.agent/tool] => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc-in [:agent-state :tools] (vec tools))
         (state runtime)))

(>defn replace-messages!
       "Replaces runtime transcript messages."
       [runtime messages]
       [:map [:vector :llx.agent/message] => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc-in [:agent-state :messages] (vec messages))
         (state runtime)))

(>defn append-message!
       "Appends one message to runtime transcript."
       [runtime message]
       [:map :llx.agent/message => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* update-in [:agent-state :messages] conj message)
         (state runtime)))

(>defn clear-messages!
       "Clears runtime transcript messages."
       [runtime]
       [:map => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc-in [:agent-state :messages] [])
         (state runtime)))

(>defn session-id
       "Returns configured session id for default runner calls."
       [runtime]
       [:map => [:maybe :llx/id-string]]
       (let [{:keys [runtime-state*]} (runtime-handle runtime)]
         (:session-id @runtime-state*)))

(>defn set-session-id!
       "Sets session id for subsequent default runner calls."
       [runtime session-id]
       [:map [:maybe :llx/id-string] => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc :session-id session-id)
         (state runtime)))

(>defn thinking-budgets
       "Returns configured thinking budgets."
       [runtime]
       [:map => [:maybe :llx.agent/thinking-budgets]]
       (let [{:keys [runtime-state*]} (runtime-handle runtime)]
         (:thinking-budgets @runtime-state*)))

(>defn set-thinking-budgets!
       "Sets thinking budgets for subsequent default runner calls."
       [runtime thinking-budgets]
       [:map [:maybe :llx.agent/thinking-budgets] => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc :thinking-budgets thinking-budgets)
         (state runtime)))

(>defn max-retry-delay-ms
       "Returns configured max retry delay (ms)."
       [runtime]
       [:map => [:maybe :llx/non-neg-int]]
       (let [{:keys [runtime-state*]} (runtime-handle runtime)]
         (:max-retry-delay-ms @runtime-state*)))

(>defn set-max-retry-delay-ms!
       "Sets max retry delay (ms) for subsequent default runner calls."
       [runtime max-retry-delay-ms]
       [:map [:maybe :llx/non-neg-int] => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc :max-retry-delay-ms max-retry-delay-ms)
         (state runtime)))

(>defn set-steering-mode!
       "Updates steering queue dequeue mode (`:one-at-a-time` or `:all`)."
       [runtime mode]
       [:map :llx.agent/queue-mode => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (ensure-valid-mode mode)
         (swap! runtime-state* assoc :steering-mode mode)
         (state runtime)))

(>defn set-follow-up-mode!
       "Updates follow-up queue dequeue mode (`:one-at-a-time` or `:all`)."
       [runtime mode]
       [:map :llx.agent/queue-mode => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (ensure-valid-mode mode)
         (swap! runtime-state* assoc :follow-up-mode mode)
         (state runtime)))

(>defn clear-steering-queue!
       "Clears queued steering messages."
       [runtime]
       [:map => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc :steering-queue [])
         (state runtime)))

(>defn clear-follow-up-queue!
       "Clears queued follow-up messages."
       [runtime]
       [:map => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc :follow-up-queue [])
         (state runtime)))

(>defn clear-all-queues!
       "Clears both steering and follow-up queues."
       [runtime]
       [:map => :llx.agent/runtime-state-snapshot]
       (let [{:keys [runtime-state*] :as runtime} (runtime-handle runtime)]
         (swap! runtime-state* assoc
                :steering-queue []
                :follow-up-queue [])
         (state runtime)))

(>defn has-queued-messages?
       "Returns true when either steering or follow-up queue has entries."
       [runtime]
       [:map => :boolean]
       (let [{:keys [runtime-state*]}                 (runtime-handle runtime)
             {:keys [steering-queue follow-up-queue]} @runtime-state*]
         (boolean
          (or (seq steering-queue)
              (seq follow-up-queue)))))
