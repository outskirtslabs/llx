(ns llx.agent.runtime
  (:refer-clojure :exclude [reset!])
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn-]]
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

(defn- normalize-messages
  [message-or-messages]
  (cond
    (nil? message-or-messages) []
    (vector? message-or-messages) message-or-messages
    :else [message-or-messages]))

(defn- busy-runtime-error
  []
  (ex-info
   "Runtime is already processing a prompt or continue command."
   {:type :llx.agent/runtime-busy}))

(defn- no-messages-error
  []
  (ex-info
   "Cannot continue: no messages in runtime state."
   {:type :llx.agent/runtime-no-messages}))

(defn- no-queued-messages-error
  []
  (ex-info
   "No queued steering or follow-up messages available for continue."
   {:type :llx.agent/runtime-no-queued-messages}))

(defn- runtime-closed-error
  []
  (ex-info
   "Runtime closed while turn was active."
   {:type :llx.agent/runtime-closed}))

(defn- runtime-reset-error
  []
  (ex-info
   "Runtime reset while turn was active."
   {:type :llx.agent/runtime-reset}))

(defn- runtime-closed-wait-error
  []
  (ex-info
   "Runtime closed while waiting for idle."
   {:type :llx.agent/runtime-closed}))

(def command-type-key
  :llx.agent.command/type)

(defn- ensure-valid-mode
  [mode]
  (if (#{:all :one-at-a-time} mode)
    mode
    (throw (ex-info "Invalid queue mode." {:mode mode}))))

(defn ->queue
  ([] #?(:clj clojure.lang.PersistentQueue/EMPTY
         :cljs #queue []))
  ([coll]
   (into (->queue) coll)))

(defn- dequeue-queued-messages!
  [runtime-state* queue-key mode]
  (let [dequeued* (volatile! [])]
    (swap! runtime-state*
           (fn [runtime-state]
             (let [queue                            (or (get runtime-state queue-key) (->queue))
                   [dequeued next-queue]
                   (if (= :all mode)
                     [(vec queue) (->queue)]
                     (if (seq queue)
                       [[(peek queue)] (pop queue)]
                       [[] queue]))]
               (vreset! dequeued* dequeued)
               (assoc runtime-state queue-key next-queue))))
    @dequeued*))

(defn- enqueue-messages!
  [runtime-state* queue-key messages]
  (swap! runtime-state* update queue-key
         (fn [queue]
           (into (or queue (->queue)) messages)))
  true)

(defn- state->snapshot
  [{:keys [agent-state steering-queue follow-up-queue steering-mode follow-up-mode]}]
  (assoc agent-state
         :steering-queue (vec steering-queue)
         :follow-up-queue (vec follow-up-queue)
         :steering-mode steering-mode
         :follow-up-mode follow-up-mode))

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

(defn- resolve-idle-waiters!
  [runtime-state*]
  (let [waiters (:idle-waiters @runtime-state*)]
    (swap! runtime-state* assoc :idle-waiters [])
    (run! (fn [{:keys [resolve]}]
            (safe-resolve resolve true))
          waiters)
    nil))

(defn- reject-idle-waiters!
  [runtime-state* ex]
  (let [waiters (:idle-waiters @runtime-state*)]
    (swap! runtime-state* assoc :idle-waiters [])
    (run! (fn [{:keys [reject]}]
            (safe-reject reject ex))
          waiters)
    nil))

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

(defn- emit-event!
  [{:keys [runtime-state* events-mx]} turn-id event]
  (if (= turn-id (:active-turn-id @runtime-state*))
    (do
      (swap! runtime-state* update :agent-state apply-event event)
      (-> (sp/put events-mx event)
          (p/then boolean)))
    (p/resolved false)))

(defn- finish-turn!
  [runtime-state* turn-id error-text]
  (let [finished?* (atom false)]
    (swap! runtime-state*
           (fn [runtime-state]
             (if (= turn-id (:active-turn-id runtime-state))
               (let [next-state (-> runtime-state
                                    (assoc :active-turn-id nil
                                           :abort-fn nil
                                           :active-turn-reject nil)
                                    (update :agent-state
                                            (fn [agent-state]
                                              (-> agent-state
                                                  (assoc :streaming? false
                                                         :stream-message nil
                                                         :pending-tool-calls #{}
                                                         :error error-text)))))]
                 (clojure.core/reset! finished?* true)
                 next-state)
               runtime-state)))
    (when @finished?*
      (resolve-idle-waiters! runtime-state*))
    @finished?*))

(defn- allocate-turn-id!
  [runtime-state*]
  (:next-turn-id
   (swap! runtime-state* update :next-turn-id (fnil inc 0))))

(>defn- validate-turn-result-value
        [turn-value]
        [:llx.agent/runtime-turn-value => :llx.agent/runtime-turn-value]
        turn-value)

(defn- begin-turn!
  "Starts a runtime turn, marks streaming state, and wires completion callbacks.

  `run-command!` must return `{:result deferred :cancel! fn}`, and the deferred
  must resolve to `:llx.agent/runtime-turn-value`."
  [{:keys [runtime-state* run-command!] :as runtime}
   {:keys [command messages skip-initial-steering-poll? resolve reject]}]
  (let [turn-id    (allocate-turn-id! runtime-state*)
        turn-state (state->snapshot @runtime-state*)
        messages   (when (seq messages) (vec messages))
        emit-event (fn [event]
                     (emit-event! runtime turn-id event))]
    (swap! runtime-state*
           (fn [runtime-state]
             (-> runtime-state
                 (assoc :active-turn-id turn-id
                        :abort-fn nil
                        :active-turn-reject reject)
                 (update :agent-state
                         (fn [agent-state]
                           (-> agent-state
                               (assoc :streaming? true
                                      :stream-message nil
                                      :error nil
                                      :pending-tool-calls #{})))))))
    (try
      (let [{:keys [result cancel!]}
            (agent-schema/validate!
             :llx.agent/runtime-turn-result
             (run-command! {:command                     command
                            :messages                    messages
                            :skip-initial-steering-poll? (boolean skip-initial-steering-poll?)
                            :state                       turn-state
                            :emit-event!                 emit-event}))]
        (swap! runtime-state* assoc :abort-fn cancel!)
        (-> result
            (p/then (fn [value]
                      (let [value (validate-turn-result-value value)]
                        (when (finish-turn! runtime-state* turn-id nil)
                          (safe-resolve resolve value)))))
            (p/catch (fn [ex]
                       (when (finish-turn! runtime-state* turn-id
                                           (or (ex-message ex) "Runtime turn command failed."))
                         (safe-reject reject ex))))))
      (catch #?(:clj Exception :cljs :default) ex
        (when (finish-turn! runtime-state* turn-id
                            (or (ex-message ex) "Runtime turn command failed."))
          (safe-reject reject ex))))
    nil))

(>defn- handle-prompt!
        [{:keys [runtime-state*] :as runtime} {:keys [messages resolve reject]}]
        [:llx.agent/runtime-handle-turn :llx.agent/command-prompt => :llx/deferred]
        (let [messages    (normalize-messages messages)
              agent-state (:agent-state @runtime-state*)]
          (cond
            (:streaming? agent-state)
            (safe-reject reject (busy-runtime-error))

            (empty? messages)
            (safe-reject reject (ex-info "Prompt requires one or more messages."
                                         {:type :llx.agent/runtime-empty-prompt}))

            :else
            (begin-turn! runtime {:command  :prompt
                                  :messages messages
                                  :resolve  resolve
                                  :reject   reject})))
        (p/resolved nil))

(>defn- handle-continue!
        [{:keys [runtime-state*] :as runtime} {:keys [resolve reject]}]
        [:llx.agent/runtime-handle-turn :llx.agent/command-continue => :llx/deferred]
        (let [{:keys [agent-state steering-mode follow-up-mode]}
              @runtime-state*
              messages                                           (:messages agent-state)
              busy?                                              (:streaming? agent-state)]
          (cond
            busy?
            (safe-reject reject (busy-runtime-error))

            (empty? messages)
            (safe-reject reject (no-messages-error))

            (= :assistant (:role (last messages)))
            (let [steering-dequeued
                  (dequeue-queued-messages! runtime-state*
                                            :steering-queue
                                            steering-mode)]
              (if (seq steering-dequeued)
                (begin-turn! runtime {:command                     :continue
                                      :messages                    steering-dequeued
                                      :skip-initial-steering-poll? true
                                      :resolve                     resolve
                                      :reject                      reject})
                (let [follow-up-dequeued
                      (dequeue-queued-messages! runtime-state*
                                                :follow-up-queue
                                                follow-up-mode)]
                  (if (seq follow-up-dequeued)
                    (begin-turn! runtime {:command  :continue
                                          :messages follow-up-dequeued
                                          :resolve  resolve
                                          :reject   reject})
                    (safe-reject reject (no-queued-messages-error))))))

            :else
            (begin-turn! runtime {:command :continue
                                  :resolve resolve
                                  :reject  reject})))
        (p/resolved nil))

(>defn- handle-steer!
        [{:keys [runtime-state*]} {:keys [messages resolve reject]}]
        [:llx.agent/runtime-handle-state :llx.agent/command-steer => :llx/deferred]
        (let [messages (normalize-messages messages)]
          (try
            (enqueue-messages! runtime-state*
                               :steering-queue
                               messages)
            (safe-resolve resolve true)
            (catch #?(:clj Exception :cljs :default) ex
              (safe-reject reject ex))))
        (p/resolved nil))

(>defn- handle-follow-up!
        [{:keys [runtime-state*]} {:keys [messages resolve reject]}]
        [:llx.agent/runtime-handle-state :llx.agent/command-follow-up => :llx/deferred]
        (let [messages (normalize-messages messages)]
          (try
            (enqueue-messages! runtime-state*
                               :follow-up-queue
                               messages)
            (safe-resolve resolve true)
            (catch #?(:clj Exception :cljs :default) ex
              (safe-reject reject ex))))
        (p/resolved nil))

(>defn- handle-abort!
        [{:keys [runtime-state*]} {:keys [resolve]}]
        [:llx.agent/runtime-handle-state :llx.agent/command-abort => :llx/deferred]
        (let [{:keys [abort-fn agent-state]} @runtime-state*]
          (if (:streaming? agent-state)
            (do
              (when (fn? abort-fn)
                (abort-fn))
              (safe-resolve resolve true))
            (safe-resolve resolve false)))
        (p/resolved nil))

(>defn- handle-reset!
        [{:keys [runtime-state*]} {:keys [resolve]}]
        [:llx.agent/runtime-handle-state :llx.agent/command-reset => :llx/deferred]
        (let [{:keys [agent-state abort-fn active-turn-id active-turn-reject
                      next-turn-id steering-mode follow-up-mode]}
              @runtime-state*

              preserved-agent-state
              (select-keys agent-state
                           [:system-prompt
                            :model
                            :thinking-level
                            :tools])]
          (when (fn? abort-fn)
            (abort-fn))
          (when active-turn-id
            (let [reset-ex (runtime-reset-error)]
              (finish-turn! runtime-state* active-turn-id (ex-message reset-ex))
              (safe-reject active-turn-reject reset-ex)))
          (swap! runtime-state*
                 (fn [runtime-state]
                   (assoc runtime-state
                          :agent-state (merge default-agent-state
                                              preserved-agent-state)
                          :steering-queue (->queue)
                          :follow-up-queue (->queue)
                          :next-turn-id next-turn-id
                          :steering-mode steering-mode
                          :follow-up-mode follow-up-mode
                          :active-turn-id nil
                          :abort-fn nil
                          :active-turn-reject nil
                          :idle-waiters [])))
          (resolve-idle-waiters! runtime-state*)
          (safe-resolve resolve (state->snapshot @runtime-state*)))
        (p/resolved nil))

(>defn- handle-wait!
        [{:keys [runtime-state*]} {:keys [resolve reject]}]
        [:llx.agent/runtime-handle-state :llx.agent/command-wait => :llx/deferred]
        (if (get-in @runtime-state* [:agent-state :streaming?])
          (swap! runtime-state* update :idle-waiters conj {:resolve resolve :reject reject})
          (safe-resolve resolve true))
        (p/resolved nil))

(defn- close-command-channels!
  [channels]
  (run! sp/close
        [(:commands channels)]))

(>defn- handle-command!
        [{:keys [channels events-mx runtime-state*] :as runtime} command]
        [:llx.agent/runtime-handle-dispatch :llx.agent/command => [:or :llx/deferred [:= :stop]]]
        (let [command-type (get command command-type-key)
              resolve      (:resolve command)
              reject       (:reject command)]
          (case command-type
            :llx.agent.command/prompt
            (handle-prompt! runtime command)

            :llx.agent.command/continue
            (handle-continue! runtime command)

            :llx.agent.command/steer
            (handle-steer! runtime command)

            :llx.agent.command/follow-up
            (handle-follow-up! runtime command)

            :llx.agent.command/abort
            (handle-abort! runtime command)

            :llx.agent.command/reset
            (handle-reset! runtime command)

            :llx.agent.command/wait
            (handle-wait! runtime command)

            :llx.agent.command/shutdown
            (do
              (let [{:keys [active-turn-id abort-fn active-turn-reject]} @runtime-state*
                    closed-ex                                            (runtime-closed-error)]
                (when (fn? abort-fn)
                  (abort-fn))
                (reject-idle-waiters! runtime-state* (runtime-closed-wait-error))
                (when active-turn-id
                  (finish-turn! runtime-state* active-turn-id (ex-message closed-ex))
                  (safe-reject active-turn-reject closed-ex)))
              (safe-resolve resolve true)
              (close-command-channels! channels)
              (sp/close events-mx)
              :stop)

            (do
              (safe-reject reject
                           (ex-info "Unknown runtime command."
                                    {:type            :llx.agent/runtime-unknown-command
                                     command-type-key command-type}))
              (p/resolved nil)))))

(defn- start-coordinator!
  [{:keys [channels] :as runtime}]
  (p/loop []
    (p/let [command (sp/take (:commands channels))
            result  (if (nil? command)
                      :stop
                      (handle-command! runtime command))]
      (if (= :stop result)
        nil
        (p/recur)))))

(defn- submit-command!
  [{:keys [channels]} command-type payload]
  (let [commands-ch (:commands channels)
        command     (assoc payload command-type-key command-type)]
    (p/create
     (fn [resolve reject]
       (if (nil? commands-ch)
         (reject (ex-info "Runtime command channel missing." {:channel :commands}))
         (try
           (agent-schema/validate! :llx.agent/command command)
           (-> (sp/put commands-ch (assoc command
                                          :resolve resolve
                                          :reject reject))
               (p/then (fn [accepted?]
                         (when-not accepted?
                           (reject (ex-info "Runtime channel is closed."
                                            {:channel :commands})))))
               (p/catch reject))
           (catch #?(:clj Exception :cljs :default) ex
             (reject ex))))))))

(defn create-runtime
  "Creates an agent runtime coordinator.

  Options:

  | key               | description                                                                          |
  |-------------------|--------------------------------------------------------------------------------------|
  | `:run-command!`   | Required function that executes a turn and returns `{:result deferred :cancel! fn}`; deferred resolves to `{:status keyword}`. |
  | `:initial-state`  | Optional initial runtime state map.                                                  |
  | `:steering-mode`  | Optional queue mode, one of `:one-at-a-time` or `:all`.                              |
  | `:follow-up-mode` | Optional queue mode, one of `:one-at-a-time` or `:all`.                              |"
  [opts]
  (let [opts
        (agent-schema/validate! :llx.agent/runtime-options opts)
        run-command!
        (:run-command! opts)
        channels
        {:commands (sp/chan :buf 128)}
        events-mx
        (sp/mult :buf (sp/sliding-buffer 128))
        initial
        (merge default-agent-state
               (:initial-state opts))
        runtime-state*
        (atom {:agent-state        initial
               :steering-queue     (->queue)
               :follow-up-queue    (->queue)
               :steering-mode      (ensure-valid-mode (or (:steering-mode opts) :one-at-a-time))
               :follow-up-mode     (ensure-valid-mode (or (:follow-up-mode opts) :one-at-a-time))
               :next-turn-id       0
               :active-turn-id     nil
               :abort-fn           nil
               :active-turn-reject nil
               :idle-waiters       []})
        runtime
        {:run-command!   run-command!
         :runtime-state* runtime-state*
         :channels       channels
         :events-mx      events-mx}]
    (assoc runtime :coordinator (start-coordinator! runtime))))

(defn state
  [runtime]
  (state->snapshot @(-> runtime :runtime-state*)))

(defn prompt!
  [runtime message-or-messages]
  (submit-command! runtime :llx.agent.command/prompt {:messages message-or-messages}))

(defn continue!
  [runtime]
  (submit-command! runtime :llx.agent.command/continue {}))

(defn steer!
  [runtime message-or-messages]
  (submit-command! runtime :llx.agent.command/steer {:messages message-or-messages}))

(defn follow-up!
  [runtime message-or-messages]
  (submit-command! runtime :llx.agent.command/follow-up {:messages message-or-messages}))

(defn abort!
  [runtime]
  (submit-command! runtime :llx.agent.command/abort {}))

(defn wait-for-idle
  [runtime]
  (submit-command! runtime :llx.agent.command/wait {}))

(defn reset!
  [runtime]
  (submit-command! runtime :llx.agent.command/reset {}))

(defn close!
  [runtime]
  (submit-command! runtime :llx.agent.command/shutdown {}))

(defn subscribe
  [{:keys [events-mx]} handler]
  (let [sub-ch   (sp/chan :buf (sp/sliding-buffer 64))
        stopped* (atom false)]
    (sp/tap events-mx sub-ch false)
    (p/loop []
      (p/let [event (sp/take sub-ch)]
        (when (and (not @stopped*) (some? event))
          (try
            (handler event)
            (catch #?(:clj Exception :cljs :default) _
              nil))
          (p/recur))))
    (fn []
      (when (compare-and-set! stopped* false true)
        (sp/untap events-mx sub-ch)
        (sp/close sub-ch)
        true))))

(defn set-steering-mode!
  [{:keys [runtime-state*] :as runtime} mode]
  (ensure-valid-mode mode)
  (swap! runtime-state* assoc :steering-mode mode)
  (state runtime))

(defn set-follow-up-mode!
  [{:keys [runtime-state*] :as runtime} mode]
  (ensure-valid-mode mode)
  (swap! runtime-state* assoc :follow-up-mode mode)
  (state runtime))

(defn clear-steering-queue!
  [{:keys [runtime-state*] :as runtime}]
  (swap! runtime-state* assoc :steering-queue (->queue))
  (state runtime))

(defn clear-follow-up-queue!
  [{:keys [runtime-state*] :as runtime}]
  (swap! runtime-state* assoc :follow-up-queue (->queue))
  (state runtime))

(defn clear-all-queues!
  [{:keys [runtime-state*] :as runtime}]
  (swap! runtime-state* assoc
         :steering-queue (->queue)
         :follow-up-queue (->queue))
  (state runtime))

(defn has-queued-messages?
  [{:keys [runtime-state*]}]
  (let [{:keys [steering-queue follow-up-queue]} @runtime-state*]
    (or (seq steering-queue)
        (seq follow-up-queue))))
