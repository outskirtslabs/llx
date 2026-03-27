(ns ol.llx.agent.loop
  (:require
   [ol.llx.agent.fx :as fx]))

(def empty-queue
  #?(:clj  clojure.lang.PersistentQueue/EMPTY
     :cljs #queue []))

(defn- assistant-tool-calls
  [message]
  (if (vector? (:content message))
    (->> (:content message)
         (filter #(= :tool-call (:type %)))
         (mapv (fn [{:keys [id name arguments]}]
                 {:id id :name name :arguments arguments})))
    []))

(defn- now-ms [] #?(:clj (System/currentTimeMillis) :cljs (.now js/Date)))

(defn- dequeue-by-mode
  [state queue-key mode-key]
  (let [queue (get state queue-key)
        mode  (get state mode-key)]
    (if-not (seq queue)
      [nil state]
      (if (= mode :all)
        [(vec queue) (assoc state queue-key empty-queue)]
        [[(peek queue)] (assoc state queue-key (pop queue))]))))

(defn- dequeue-continuation-messages
  [state]
  (let [[steering state-with-steering] (dequeue-by-mode state :steering-queue :steering-mode)]
    (if (seq steering)
      [steering state-with-steering]
      (dequeue-by-mode state-with-steering :follow-up-queue :follow-up-mode))))

(defn- message-lifecycle-effects
  [message]
  [{::fx/type :emit-event :event {:type :ol.llx.agent.event/message-start :message message}}
   {::fx/type :emit-event :event {:type :ol.llx.agent.event/message-end :message message}}])

(defn- tool-start-effect
  [tool-call]
  {::fx/type :emit-event
   :event    {:type         :ol.llx.agent.event/tool-execution-start
              :tool-call-id (:id tool-call)
              :tool-name    (:name tool-call)
              :args         (:arguments tool-call)}})

(def ^:private skipped-tool-result-text
  "Skipped due to queued user message.")

(defn- skipped-tool-result-message
  [tool-call]
  {:role         :tool-result
   :tool-call-id (:id tool-call)
   :tool-name    (:name tool-call)
   :content      [{:type :text
                   :text skipped-tool-result-text}]
   :is-error?    true
   :timestamp    (now-ms)})

(defn- tool-execution-end-event
  [tool-result]
  {:type         :ol.llx.agent.event/tool-execution-end
   :tool-call-id (:tool-call-id tool-result)
   :tool-name    (:tool-name tool-result)
   :result       tool-result
   :is-error?    (:is-error? tool-result)})

(defn- tool-result-lifecycle-effects
  [tool-result]
  (into [{::fx/type :emit-event :event (tool-execution-end-event tool-result)}]
        (message-lifecycle-effects tool-result)))

(defn- skip-tool-call-effects
  [tool-call]
  (let [tool-result (skipped-tool-result-message tool-call)]
    {:tool-result tool-result
     :effects     (into [(tool-start-effect tool-call)]
                        (tool-result-lifecycle-effects tool-result))}))

(defn- tool-complete-transition
  [state tool-result]
  (let [remaining                   (rest (:pending-tool-calls state))
        next-tool                   (first remaining)
        state'                      (-> state
                                        (update :messages conj tool-result)
                                        (assoc :pending-tool-calls (vec remaining)))
        [steering-messages state''] (dequeue-by-mode state' :steering-queue :steering-mode)]
    (cond
      (seq steering-messages)
      (let [skipped              (mapv skip-tool-call-effects remaining)
            skipped-tool-results (mapv :tool-result skipped)
            skipped-effects      (mapcat :effects skipped)
            steering-effects     (mapcat message-lifecycle-effects steering-messages)
            state'''             (-> state''
                                     (update :messages into skipped-tool-results)
                                     (update :messages into steering-messages)
                                     (assoc :pending-tool-calls []))
            completion-effects   (tool-result-lifecycle-effects tool-result)]
        [state'''
         (into []
               (concat completion-effects
                       skipped-effects
                       steering-effects
                       [{::fx/type :call-llm :messages (:messages state''')}]))])

      (seq remaining)
      [state'
       (into []
             (concat (tool-result-lifecycle-effects tool-result)
                     [(tool-start-effect next-tool)
                      {::fx/type :execute-tool :tool-call next-tool}]))]

      :else
      [state'
       (into []
             (concat (tool-result-lifecycle-effects tool-result)
                     [{::fx/type :call-llm :messages (:messages state')}]))])))

(defn- continue-after-turn
  [state completion-effects]
  (let [[queued-messages state'] (dequeue-continuation-messages state)]
    (if (seq queued-messages)
      (let [state'' (-> state'
                        (update :messages into queued-messages)
                        (assoc ::phase ::streaming))]
        [state''
         (into []
               (concat completion-effects
                       [{::fx/type :emit-event :event {:type :ol.llx.agent.event/turn-start}}]
                       (mapcat message-lifecycle-effects queued-messages)
                       [{::fx/type :call-llm :messages (:messages state'')}]))])
      [state completion-effects])))

(defn handle-command
  "Pure command handler. Takes state + command, returns `[state' signals]`.
   Handles global state mutations directly and translates FSM-driving
   commands into signals for the graph.

   Commands that only mutate state (e.g. `:ol.llx.agent.command/steer`) return an empty
   signals vector. Commands that drive the FSM (e.g. `:ol.llx.agent.command/prompt`)
   return one or more signals to be fed into the transition functions."
  [state cmd]
  (case (:type cmd)
    :ol.llx.agent.command/prompt
    (if (not= ::idle (::phase state))
      [state [{:type :ol.llx.agent.signal/rejected :reason :not-idle}]]
      [state [{:type :ol.llx.agent.signal/prompt-start :messages (:messages cmd)}]])

    :ol.llx.agent.command/continue
    (if (not= ::idle (::phase state))
      [state [{:type :ol.llx.agent.signal/rejected :reason :not-idle}]]
      (let [[messages state'] (dequeue-continuation-messages state)]
        (if (seq messages)
          [state' [{:type :ol.llx.agent.signal/continue-start :messages messages}]]
          [state [{:type :ol.llx.agent.signal/rejected :reason :no-queued-messages}]])))

    :ol.llx.agent.command/abort
    (if (= ::idle (::phase state))
      [state [{:type :ol.llx.agent.signal/rejected :reason :idle}]]
      [state [{:type :ol.llx.agent.signal/abort}]])

    :ol.llx.agent.command/steer
    [(update state :steering-queue conj (:message cmd)) []]

    :ol.llx.agent.command/follow-up
    [(update state :follow-up-queue conj (:message cmd)) []]

    :ol.llx.agent.command/set-system-prompt
    [(assoc state :system-prompt (:system-prompt cmd)) []]

    :ol.llx.agent.command/set-model
    [(assoc state :model (:model cmd)) []]

    :ol.llx.agent.command/set-thinking-level
    [(assoc state :thinking-level (:thinking-level cmd)) []]

    :ol.llx.agent.command/set-tools
    [(assoc state :tools (:tools cmd)) []]

    :ol.llx.agent.command/set-steering-mode
    [(assoc state :steering-mode (:mode cmd)) []]

    :ol.llx.agent.command/set-follow-up-mode
    [(assoc state :follow-up-mode (:mode cmd)) []]

    :ol.llx.agent.command/replace-messages
    [(assoc state :messages (:messages cmd)) []]

    :ol.llx.agent.command/append-message
    [(update state :messages conj (:message cmd)) []]

    :ol.llx.agent.command/clear-messages
    [(assoc state :messages []) []]

    :ol.llx.agent.command/clear-steering-queue
    [(assoc state :steering-queue empty-queue) []]

    :ol.llx.agent.command/clear-follow-up-queue
    [(assoc state :follow-up-queue empty-queue) []]

    :ol.llx.agent.command/clear-all-queues
    [(-> state
         (assoc :steering-queue empty-queue)
         (assoc :follow-up-queue empty-queue)) []]

    :ol.llx.agent.command/reset
    [(-> state
         (assoc ::phase ::idle)
         (assoc :messages [])
         (assoc :steering-queue empty-queue)
         (assoc :follow-up-queue empty-queue)
         (assoc :stream-message nil)
         (assoc :pending-tool-calls [])
         (assoc :error nil)) []]))

(defn idle-transition
  "Pure transition from ::idle state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :ol.llx.agent.signal/prompt-start
    (let [messages' (into (:messages state) (:messages msg))]
      [(-> state
           (assoc :messages messages')
           (assoc ::phase ::streaming))
       (into [{::fx/type :emit-event :event {:type :ol.llx.agent.event/agent-start}}
              {::fx/type :emit-event :event {:type :ol.llx.agent.event/turn-start}}]
             (concat
              (mapcat (fn [m] [{::fx/type :emit-event :event {:type :ol.llx.agent.event/message-start :message m}}
                               {::fx/type :emit-event :event {:type :ol.llx.agent.event/message-end :message m}}])
                      (:messages msg))
              [{::fx/type :call-llm :messages messages'}]))])

    :ol.llx.agent.signal/continue-start
    (let [messages' (into (:messages state) (:messages msg))]
      [(-> state
           (assoc :messages messages')
           (assoc ::phase ::streaming))
       (into [{::fx/type :emit-event :event {:type :ol.llx.agent.event/turn-start}}]
             (concat
              (mapcat (fn [m] [{::fx/type :emit-event :event {:type :ol.llx.agent.event/message-start :message m}}
                               {::fx/type :emit-event :event {:type :ol.llx.agent.event/message-end :message m}}])
                      (:messages msg))
              [{::fx/type :call-llm :messages messages'}]))])

    [state [{::fx/type :reject :reason :invalid-signal}]]))

(defn streaming-transition
  "Pure transition from ::streaming state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :ol.llx.agent.signal/llm-start
    [(assoc state :stream-message (:message msg))
     [{::fx/type :emit-event
       :event    {:type :ol.llx.agent.event/message-start :message (:message msg)}}]]

    :ol.llx.agent.signal/llm-chunk
    [(assoc state :stream-message (:chunk msg))
     [{::fx/type :emit-event
       :event    {:type :ol.llx.agent.event/message-update :chunk (:chunk msg)}}]]

    :ol.llx.agent.signal/llm-done
    (let [message    (:message msg)
          tool-calls (assistant-tool-calls message)
          first-tool (first tool-calls)]
      (if (seq tool-calls)
        [(-> state
             (update :messages conj message)
             (assoc ::phase ::tool-executing)
             (assoc :stream-message nil)
             (assoc :pending-tool-calls (vec tool-calls)))
         [{::fx/type :emit-event :event {:type :ol.llx.agent.event/message-end :message message}}
          {::fx/type :emit-event :event {:type         :ol.llx.agent.event/tool-execution-start
                                         :tool-call-id (:id first-tool)
                                         :tool-name    (:name first-tool)
                                         :args         (:arguments first-tool)}}
          {::fx/type :execute-tool :tool-call first-tool}]]
        (continue-after-turn
         (-> state
             (update :messages conj message)
             (assoc ::phase ::idle)
             (assoc :stream-message nil))
         [{::fx/type :emit-event :event {:type :ol.llx.agent.event/message-end :message message}}
          {::fx/type :emit-event :event {:type :ol.llx.agent.event/turn-end :message message}}])))

    :ol.llx.agent.signal/llm-error
    [(-> state
         (assoc ::phase ::idle)
         (assoc :error (:error msg)))
     [{::fx/type :emit-event :event {:type :ol.llx.agent.event/message-end :message {:stop-reason :error}}}
      {::fx/type :emit-event :event {:type :ol.llx.agent.event/turn-end}}]]

    :ol.llx.agent.signal/abort
    [(-> state
         (assoc ::phase ::idle)
         (assoc :stream-message nil))
     [{::fx/type :emit-event :event {:type :ol.llx.agent.event/message-end :message {:stop-reason :aborted}}}
      {::fx/type :emit-event :event {:type :ol.llx.agent.event/turn-end}}]]

    [state []]))

(defn tool-executing-transition
  "Pure transition from ::tool-executing state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :ol.llx.agent.signal/tool-result
    (tool-complete-transition state (:tool-result-message msg))

    :ol.llx.agent.signal/tool-error
    (tool-complete-transition state (:tool-result-message msg))

    :ol.llx.agent.signal/tool-update
    [state
     [{::fx/type :emit-event
       :event    {:type           :ol.llx.agent.event/tool-execution-update
                  :tool-call-id   (:tool-call-id msg)
                  :tool-name      (:tool-name msg)
                  :partial-result (:partial-result msg)}}]]

    :ol.llx.agent.signal/abort
    [(-> state
         (assoc ::phase ::idle)
         (assoc :pending-tool-calls []))
     [{::fx/type :emit-event :event {:type :ol.llx.agent.event/turn-end}}]]

    [state []]))

(defn closed-transition
  "Terminal state. No transitions possible."
  [state _msg]
  [state []])

(defn route-from-idle [state]
  (::phase state))

(defn route-from-streaming [state]
  (::phase state))

(defn route-from-tool-executing [state]
  (case (::phase state)
    ::idle ::idle
    ::closed ::closed
    (if (seq (:pending-tool-calls state))
      ::tool-executing
      ::streaming)))

(defn command?
  [input]
  (= "ol.llx.agent.command" (namespace (:type input))))

(defn- step-signal
  "Process a single signal through the graph. Pure.
   Runs the transition fn for the current phase, then the routing fn
   to determine the next phase. Returns `[state' effects]`."
  [g state sig]
  (let [current-phase    (::phase state)
        transition-fn    (get-in g [::transitions current-phase])
        routing-fn       (get-in g [::routes current-phase])
        [state' effects] (transition-fn state sig)
        next-phase       (routing-fn state')
        prev-phase       current-phase
        state''          (assoc state' ::phase next-phase)
        agent-ended?     (and (not= ::idle prev-phase)
                              (= ::idle next-phase))]
    (if agent-ended?
      [state'' (conj effects {::fx/type :emit-event
                              :event    {:type     :ol.llx.agent.event/agent-end
                                         :messages (:messages state'')}})]
      [state'' effects])))

(def graph
  "Agent state machine as data.

   Transitions: `(state, msg) -> [state', effects]`
     Pure transition fns. Take current state and a signal, return
     new state and a vector of effect descriptions to execute.

   Routes: `(state') -> phase-key`
     Pure routing fns. Take the post-transition state and return
     the keyword of the next phase to enter."
  {::transitions {::idle           idle-transition
                  ::streaming      streaming-transition
                  ::tool-executing tool-executing-transition
                  ::closed         closed-transition}

   ::routes      {::idle           route-from-idle
                  ::streaming      route-from-streaming
                  ::tool-executing route-from-tool-executing
                  ::closed         (constantly ::closed)}})

(defn step
  "Pure step function. Takes state + input (command or signal), returns `[state' effects]`.

   Commands are processed by `handle-command` which may produce signals.
   Signals are processed by the graph transition + routing fns.

   Emits `:ol.llx.agent.event/agent-end` automatically when the agent transitions from a non-idle phase back to `::idle`."
  ([state input]
   (step graph state input))
  ([g state input]
   (if (command? input)
     (let [[state' sigs] (handle-command state input)]
       (reduce
        (fn [[acc-state acc-effects] sig]
          (let [[state'' effects] (step-signal g acc-state sig)]
            [state'' (into acc-effects effects)]))
        [state' []]
        sigs))
     (step-signal g state input))))
