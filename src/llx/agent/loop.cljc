(ns llx.agent.loop
  (:require
   [llx.agent.fx :as fx]))

(def empty-queue
  #?(:clj  clojure.lang.PersistentQueue/EMPTY
     :cljs #queue []))

(def states
  "The agent's possible states"
  #{::idle ;; no inference loop is running, awaiting prompt, continue, or abort
    ::streaming ;; actively streaming an llm inference response
    ::tool-executing ;; a tool call is in progress
    ::closed ;; terminal state
    })

(def commands
  "Commands from the caller into the agent."
  #{:llx.agent.command/prompt ;; start inference with new messages
    :llx.agent.command/continue ;; resume with queued steering or follow-up messages
    :llx.agent.command/abort ;; cancel the running loop
    :llx.agent.command/steer ;; inject a steering message mid-run
    :llx.agent.command/follow-up ;; queue a message for after the agent finishes
    :llx.agent.command/set-system-prompt ;; replace the system prompt
    :llx.agent.command/set-model ;; change the LLM model
    :llx.agent.command/set-thinking-level ;; set reasoning level (off, low, medium, high, etc.)
    :llx.agent.command/set-tools ;; replace the available tool set
    :llx.agent.command/set-steering-mode ;; set steering dequeue mode (all or one-at-a-time)
    :llx.agent.command/set-follow-up-mode ;; set follow-up dequeue mode (all or one-at-a-time)
    :llx.agent.command/replace-messages ;; replace the entire message history
    :llx.agent.command/append-message ;; append a single message to history
    :llx.agent.command/clear-messages ;; clear the message history
    :llx.agent.command/clear-steering-queue ;; drop all queued steering messages
    :llx.agent.command/clear-follow-up-queue ;; drop all queued follow-up messages
    :llx.agent.command/clear-all-queues ;; drop all queued steering and follow-up messages
    :llx.agent.command/reset ;; clear all state (messages, queues, errors)
    })

(def signals
  "Internal signals fed back into the TEA loop by the effect interpreter
   or produced by the command handler."
  #{:llx.agent.signal/prompt-start ;; command/prompt accepted, begin inference
    :llx.agent.signal/continue-start ;; command/continue accepted with dequeued messages
    :llx.agent.signal/abort ;; command/abort accepted, cancel running loop
    :llx.agent.signal/rejected ;; command was rejected (carries :reason)
    :llx.agent.signal/llm-start ;; LLM stream begun, initial partial message available
    :llx.agent.signal/llm-chunk ;; streaming chunk received from LLM
    :llx.agent.signal/llm-done ;; LLM inference completed
    :llx.agent.signal/llm-error ;; LLM inference failed
    :llx.agent.signal/tool-result ;; tool execution completed successfully
    :llx.agent.signal/tool-error ;; tool execution failed
    :llx.agent.signal/tool-update ;; tool execution progress update
    })

(def events
  "Lifecycle events emitted from the agent to subscribers."
  #{:llx.agent.event/agent-start ;; agent begins processing
    :llx.agent.event/agent-end ;; agent completes with all new messages
    :llx.agent.event/turn-start ;; new turn begins (one LLM call + tool executions)
    :llx.agent.event/turn-end ;; turn completes with assistant message and tool results
    :llx.agent.event/message-start ;; any message begins (user, assistant, tool-result)
    :llx.agent.event/message-update ;; assistant streaming chunk update
    :llx.agent.event/message-end ;; message fully received/processed
    :llx.agent.event/tool-execution-start ;; tool begins executing
    :llx.agent.event/tool-execution-update ;; tool streams progress
    :llx.agent.event/tool-execution-end ;; tool execution finished (success or error)
    })

(defn handle-command
  "Pure command handler. Takes state + command, returns `[state' signals]`.
   Handles global state mutations directly and translates FSM-driving
   commands into signals for the TEA engine.

   Commands that only mutate state (e.g. `:llx.agent.command/steer`) return an empty
   signals vector. Commands that drive the FSM (e.g. `:llx.agent.command/prompt`)
   return one or more signals to be fed into the transition functions."
  [state cmd]
  (case (:type cmd)
    :llx.agent.command/prompt
    (if (not= ::idle (::phase state))
      [state [{:type :llx.agent.signal/rejected :reason :not-idle}]]
      [state [{:type :llx.agent.signal/prompt-start :messages (:messages cmd)}]])

    :llx.agent.command/continue
    (if (not= ::idle (::phase state))
      [state [{:type :llx.agent.signal/rejected :reason :not-idle}]]
      (let [{:keys [steering-queue follow-up-queue
                    steering-mode follow-up-mode]}                                         state
            [msgs queue-key updated-queue]
            (cond
              (seq steering-queue)
              [(if (= steering-mode :all) (vec steering-queue) [(peek steering-queue)])
               :steering-queue
               (if (= steering-mode :all) empty-queue (pop steering-queue))]

              (seq follow-up-queue)
              [(if (= follow-up-mode :all) (vec follow-up-queue) [(peek follow-up-queue)])
               :follow-up-queue
               (if (= follow-up-mode :all) empty-queue (pop follow-up-queue))]

              :else nil)]
        (if msgs
          [(assoc state queue-key updated-queue)
           [{:type :llx.agent.signal/continue-start :messages msgs}]]
          [state [{:type :llx.agent.signal/rejected :reason :no-queued-messages}]])))

    :llx.agent.command/abort
    (if (= ::idle (::phase state))
      [state [{:type :llx.agent.signal/rejected :reason :idle}]]
      [state [{:type :llx.agent.signal/abort}]])

    :llx.agent.command/steer
    [(update state :steering-queue conj (:message cmd))
     []]

    :llx.agent.command/follow-up
    [(update state :follow-up-queue conj (:message cmd))
     []]

    :llx.agent.command/set-system-prompt
    [(assoc state :system-prompt (:system-prompt cmd))
     []]

    :llx.agent.command/set-model
    [(assoc state :model (:model cmd))
     []]

    :llx.agent.command/set-thinking-level
    [(assoc state :thinking-level (:thinking-level cmd))
     []]

    :llx.agent.command/set-tools
    [(assoc state :tools (:tools cmd))
     []]

    :llx.agent.command/set-steering-mode
    [(assoc state :steering-mode (:mode cmd))
     []]

    :llx.agent.command/set-follow-up-mode
    [(assoc state :follow-up-mode (:mode cmd))
     []]

    :llx.agent.command/replace-messages
    [(assoc state :messages (:messages cmd))
     []]

    :llx.agent.command/append-message
    [(update state :messages conj (:message cmd))
     []]

    :llx.agent.command/clear-messages
    [(assoc state :messages [])
     []]

    :llx.agent.command/clear-steering-queue
    [(assoc state :steering-queue empty-queue)
     []]

    :llx.agent.command/clear-follow-up-queue
    [(assoc state :follow-up-queue empty-queue)
     []]

    :llx.agent.command/clear-all-queues
    [(-> state
         (assoc :steering-queue empty-queue)
         (assoc :follow-up-queue empty-queue))
     []]

    :llx.agent.command/reset
    [(-> state
         (assoc ::phase ::idle)
         (assoc :messages [])
         (assoc :steering-queue empty-queue)
         (assoc :follow-up-queue empty-queue)
         (assoc :stream-message nil)
         (assoc :pending-tool-calls [])
         (assoc :error nil))
     []]))

(defn idle-transition
  "Pure transition from ::idle state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :llx.agent.signal/prompt-start
    (let [messages' (into (:messages state) (:messages msg))]
      [(-> state
           (assoc :messages messages')
           (assoc ::phase ::streaming))
       (into [{::fx/type :emit-event :event {:type :llx.agent.event/agent-start}}
              {::fx/type :emit-event :event {:type :llx.agent.event/turn-start}}]
             (concat
              (mapcat (fn [m] [{::fx/type :emit-event :event {:type :llx.agent.event/message-start :message m}}
                               {::fx/type :emit-event :event {:type :llx.agent.event/message-end :message m}}])
                      (:messages msg))
              [{::fx/type :call-llm :messages messages'}]))])

    :llx.agent.signal/continue-start
    (let [messages' (into (:messages state) (:messages msg))]
      [(-> state
           (assoc :messages messages')
           (assoc ::phase ::streaming))
       (into [{::fx/type :emit-event :event {:type :llx.agent.event/turn-start}}]
             (concat
              (mapcat (fn [m] [{::fx/type :emit-event :event {:type :llx.agent.event/message-start :message m}}
                               {::fx/type :emit-event :event {:type :llx.agent.event/message-end :message m}}])
                      (:messages msg))
              [{::fx/type :call-llm :messages messages'}]))])

    [state [{::fx/type :reject :reason :invalid-signal}]]))

(defn streaming-transition
  "Pure transition from ::streaming state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :llx.agent.signal/llm-start
    [(assoc state :stream-message (:message msg))
     [{::fx/type :emit-event
       :event    {:type :llx.agent.event/message-start :message (:message msg)}}]]

    :llx.agent.signal/llm-chunk
    [(assoc state :stream-message (:chunk msg))
     [{::fx/type :emit-event
       :event    {:type :llx.agent.event/message-update :chunk (:chunk msg)}}]]

    :llx.agent.signal/llm-done
    (let [message    (:message msg)
          tool-calls (:tool-calls message)
          first-tool (first tool-calls)]
      (if (seq tool-calls)
        [(-> state
             (update :messages conj message)
             (assoc ::phase ::tool-executing)
             (assoc :stream-message nil)
             (assoc :pending-tool-calls (vec tool-calls)))
         [{::fx/type :emit-event :event {:type :llx.agent.event/message-end :message message}}
          {::fx/type :emit-event :event {:type         :llx.agent.event/tool-execution-start
                                         :tool-call-id (:id first-tool)
                                         :tool-name    (:name first-tool)
                                         :args         (:arguments first-tool)}}
          {::fx/type :execute-tool :tool-call first-tool}]]
        [(-> state
             (update :messages conj message)
             (assoc ::phase ::idle)
             (assoc :stream-message nil))
         [{::fx/type :emit-event :event {:type :llx.agent.event/message-end :message message}}
          {::fx/type :emit-event :event {:type :llx.agent.event/turn-end :message message}}]]))

    :llx.agent.signal/llm-error
    [(-> state
         (assoc ::phase ::idle)
         (assoc :error (:error msg)))
     [{::fx/type :emit-event :event {:type :llx.agent.event/message-end :message {:stop-reason :error}}}
      {::fx/type :emit-event :event {:type :llx.agent.event/turn-end}}
      {::fx/type :emit-event :event {:type :llx.agent.event/agent-end :messages (:messages state)}}]]

    :llx.agent.signal/abort
    [(-> state
         (assoc ::phase ::closed)
         (assoc :stream-message nil))
     [{::fx/type :emit-event :event {:type :llx.agent.event/message-end :message {:stop-reason :aborted}}}
      {::fx/type :emit-event :event {:type :llx.agent.event/turn-end}}
      {::fx/type :emit-event :event {:type :llx.agent.event/agent-end :messages (:messages state)}}]]

    [state []]))

(defn tool-executing-transition
  "Pure transition from ::tool-executing state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :llx.agent.signal/tool-result
    (let [result      (:result msg)
          tool-result (:tool-result-message msg)
          remaining   (rest (:pending-tool-calls state))
          next-tool   (first remaining)
          state'      (-> state
                          (update :messages conj result)
                          (assoc :pending-tool-calls (vec remaining)))]
      (if (seq remaining)
        [state'
         [{::fx/type :emit-event :event {:type :llx.agent.event/tool-execution-end :result result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/message-start :message tool-result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/message-end :message tool-result}}
          {::fx/type :emit-event :event {:type         :llx.agent.event/tool-execution-start
                                         :tool-call-id (:id next-tool)
                                         :tool-name    (:name next-tool)
                                         :args         (:arguments next-tool)}}
          {::fx/type :execute-tool :tool-call next-tool}]]
        [state'
         [{::fx/type :emit-event :event {:type :llx.agent.event/tool-execution-end :result result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/message-start :message tool-result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/message-end :message tool-result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/turn-end}}]]))

    :llx.agent.signal/tool-error
    (let [remaining    (rest (:pending-tool-calls state))
          next-tool    (first remaining)
          error-result {:role         :tool-result
                        :tool-call-id (:tool-call-id msg)
                        :error        (:error msg)}
          tool-result  (:tool-result-message msg)
          state'       (-> state
                           (update :messages conj error-result)
                           (assoc :pending-tool-calls (vec remaining)))]
      (if (seq remaining)
        [state'
         [{::fx/type :emit-event :event {:type :llx.agent.event/tool-execution-end :result error-result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/message-start :message tool-result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/message-end :message tool-result}}
          {::fx/type :emit-event :event {:type         :llx.agent.event/tool-execution-start
                                         :tool-call-id (:id next-tool)
                                         :tool-name    (:name next-tool)
                                         :args         (:arguments next-tool)}}
          {::fx/type :execute-tool :tool-call next-tool}]]
        [state'
         [{::fx/type :emit-event :event {:type :llx.agent.event/tool-execution-end :result error-result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/message-start :message tool-result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/message-end :message tool-result}}
          {::fx/type :emit-event :event {:type :llx.agent.event/turn-end}}]]))

    :llx.agent.signal/tool-update
    [state
     [{::fx/type :emit-event
       :event    {:type           :llx.agent.event/tool-execution-update
                  :tool-call-id   (:tool-call-id msg)
                  :tool-name      (:tool-name msg)
                  :partial-result (:partial-result msg)}}]]

    :llx.agent.signal/abort
    [(-> state
         (assoc ::phase ::closed)
         (assoc :pending-tool-calls []))
     [{::fx/type :emit-event :event {:type :llx.agent.event/turn-end}}
      {::fx/type :emit-event :event {:type :llx.agent.event/agent-end :messages (:messages state)}}]]

    [state []]))

(defn closed-transition
  "Terminal state. No transitions possible."
  [state _msg]
  [state []])

(defn route-from-idle [state]
  (::phase state))

(defn route-from-streaming [state]
  (case (::phase state)
    ::idle (if (or (seq (:steering-queue state))
                   (seq (:follow-up-queue state)))
             ::streaming
             ::idle)
    ::tool-executing ::tool-executing
    ::closed ::closed
    ::streaming))

(defn route-from-tool-executing [state]
  (case (::phase state)
    ::closed ::closed
    (if (seq (:pending-tool-calls state))
      ::tool-executing
      ::streaming)))

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

(defn command?
  "Returns true if `input` is a command."
  [input]
  (= "llx.agent.command" (namespace (:type input))))

(defn- step-signal
  "Process a single signal through the TEA graph. Pure.
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
                              :event    {:type     :llx.agent.event/agent-end
                                         :messages (:messages state'')}})]
      [state'' effects])))

(defn step
  "Pure TEA step function. Takes state + input (command or signal),
   returns `[state' effects]`.

   Commands are processed by `handle-command` which may produce signals.
   Signals are processed by the graph transition + routing fns.

  Emits `:llx.agent.event/agent-end` automatically when the agent transitions
   from a non-idle phase back to `::idle`."
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
