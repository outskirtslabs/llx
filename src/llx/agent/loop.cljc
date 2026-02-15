(ns llx.agent.loop
  (:require
   [llx.agent.fx :as fx]))

(def empty-queue
  #?(:clj  clojure.lang.PersistentQueue/EMPTY
     :cljs #queue []))

(def states
  "The agent's possible states"
  #{:node/idle ;; no inference loop is running, awaiting prompt, continue, or abort
    :node/streaming ;; actively streaming an llm inference response
    :node/tool-executing ;; a tool call is in progress
    :node/closed ;; terminal state
    })

(def commands
  "Commands from the caller into the agent."
  #{:command/prompt ;; start inference with new messages
    :command/continue ;; resume with queued steering or follow-up messages
    :command/abort ;; cancel the running loop
    :command/steer ;; inject a steering message mid-run
    :command/follow-up ;; queue a message for after the agent finishes
    :command/set-system-prompt ;; replace the system prompt
    :command/set-model ;; change the LLM model
    :command/set-thinking-level ;; set reasoning level (off, low, medium, high, etc.)
    :command/set-tools ;; replace the available tool set
    :command/set-steering-mode ;; set steering dequeue mode (all or one-at-a-time)
    :command/set-follow-up-mode ;; set follow-up dequeue mode (all or one-at-a-time)
    :command/replace-messages ;; replace the entire message history
    :command/append-message ;; append a single message to history
    :command/clear-messages ;; clear the message history
    :command/clear-steering-queue ;; drop all queued steering messages
    :command/clear-follow-up-queue ;; drop all queued follow-up messages
    :command/clear-all-queues ;; drop all queued steering and follow-up messages
    :command/reset ;; clear all state (messages, queues, errors)
    })

(def signals
  "Internal signals fed back into the TEA loop by the effect interpreter
   or produced by the command handler."
  #{:signal/prompt-start ;; command/prompt accepted, begin inference
    :signal/continue-start ;; command/continue accepted with dequeued messages
    :signal/abort ;; command/abort accepted, cancel running loop
    :signal/rejected ;; command was rejected (carries :reason)
    :signal/llm-start ;; LLM stream begun, initial partial message available
    :signal/llm-chunk ;; streaming chunk received from LLM
    :signal/llm-done ;; LLM inference completed
    :signal/llm-error ;; LLM inference failed
    :signal/tool-result ;; tool execution completed successfully
    :signal/tool-error ;; tool execution failed
    :signal/tool-update ;; tool execution progress update
    })

(def events
  "Lifecycle events emitted from the agent to subscribers."
  #{:event/agent-start ;; agent begins processing
    :event/agent-end ;; agent completes with all new messages
    :event/turn-start ;; new turn begins (one LLM call + tool executions)
    :event/turn-end ;; turn completes with assistant message and tool results
    :event/message-start ;; any message begins (user, assistant, tool-result)
    :event/message-update ;; assistant streaming chunk update
    :event/message-end ;; message fully received/processed
    :event/tool-execution-start ;; tool begins executing
    :event/tool-execution-update ;; tool streams progress
    :event/tool-execution-end ;; tool execution finished (success or error)
    })

(defn handle-command
  "Pure command handler. Takes state + command, returns `[state' signals]`.
   Handles global state mutations directly and translates FSM-driving
   commands into signals for the TEA engine.

   Commands that only mutate state (e.g. `:command/steer`) return an empty
   signals vector. Commands that drive the FSM (e.g. `:command/prompt`)
   return one or more signals to be fed into the transition functions."
  [state cmd]
  (case (:type cmd)
    :command/prompt
    (if (not= :node/idle (:node state))
      [state [{:type :signal/rejected :reason :not-idle}]]
      [state [{:type :signal/prompt-start :messages (:messages cmd)}]])

    :command/continue
    (if (not= :node/idle (:node state))
      [state [{:type :signal/rejected :reason :not-idle}]]
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
           [{:type :signal/continue-start :messages msgs}]]
          [state [{:type :signal/rejected :reason :no-queued-messages}]])))

    :command/abort
    (if (= :node/idle (:node state))
      [state [{:type :signal/rejected :reason :idle}]]
      [state [{:type :signal/abort}]])

    :command/steer
    [(update state :steering-queue conj (:message cmd))
     []]

    :command/follow-up
    [(update state :follow-up-queue conj (:message cmd))
     []]

    :command/set-system-prompt
    [(assoc state :system-prompt (:system-prompt cmd))
     []]

    :command/set-model
    [(assoc state :model (:model cmd))
     []]

    :command/set-thinking-level
    [(assoc state :thinking-level (:thinking-level cmd))
     []]

    :command/set-tools
    [(assoc state :tools (:tools cmd))
     []]

    :command/set-steering-mode
    [(assoc state :steering-mode (:mode cmd))
     []]

    :command/set-follow-up-mode
    [(assoc state :follow-up-mode (:mode cmd))
     []]

    :command/replace-messages
    [(assoc state :messages (:messages cmd))
     []]

    :command/append-message
    [(update state :messages conj (:message cmd))
     []]

    :command/clear-messages
    [(assoc state :messages [])
     []]

    :command/clear-steering-queue
    [(assoc state :steering-queue empty-queue)
     []]

    :command/clear-follow-up-queue
    [(assoc state :follow-up-queue empty-queue)
     []]

    :command/clear-all-queues
    [(-> state
         (assoc :steering-queue empty-queue)
         (assoc :follow-up-queue empty-queue))
     []]

    :command/reset
    [(-> state
         (assoc :node :node/idle)
         (assoc :messages [])
         (assoc :steering-queue empty-queue)
         (assoc :follow-up-queue empty-queue)
         (assoc :stream-message nil)
         (assoc :pending-tool-calls [])
         (assoc :error nil))
     []]))

(defn idle-transition
  "Pure transition from :node/idle state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :signal/prompt-start
    (let [messages' (into (:messages state) (:messages msg))]
      [(-> state
           (assoc :messages messages')
           (assoc :node :node/streaming))
       (into [{::fx/type :emit-event :event {:type :event/agent-start}}
              {::fx/type :emit-event :event {:type :event/turn-start}}]
             (concat
              (mapcat (fn [m] [{::fx/type :emit-event :event {:type :event/message-start :message m}}
                               {::fx/type :emit-event :event {:type :event/message-end :message m}}])
                      (:messages msg))
              [{::fx/type :call-llm :messages messages'}]))])

    :signal/continue-start
    (let [messages' (into (:messages state) (:messages msg))]
      [(-> state
           (assoc :messages messages')
           (assoc :node :node/streaming))
       (into [{::fx/type :emit-event :event {:type :event/turn-start}}]
             (concat
              (mapcat (fn [m] [{::fx/type :emit-event :event {:type :event/message-start :message m}}
                               {::fx/type :emit-event :event {:type :event/message-end :message m}}])
                      (:messages msg))
              [{::fx/type :call-llm :messages messages'}]))])

    [state [{::fx/type :reject :reason :invalid-signal}]]))

(defn streaming-transition
  "Pure transition from :node/streaming state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :signal/llm-start
    [(assoc state :stream-message (:message msg))
     [{::fx/type :emit-event
       :event    {:type :event/message-start :message (:message msg)}}]]

    :signal/llm-chunk
    [(assoc state :stream-message (:chunk msg))
     [{::fx/type :emit-event
       :event    {:type :event/message-update :chunk (:chunk msg)}}]]

    :signal/llm-done
    (let [message    (:message msg)
          tool-calls (:tool-calls message)
          first-tool (first tool-calls)]
      (if (seq tool-calls)
        [(-> state
             (update :messages conj message)
             (assoc :node :node/tool-executing)
             (assoc :stream-message nil)
             (assoc :pending-tool-calls (vec tool-calls)))
         [{::fx/type :emit-event :event {:type :event/message-end :message message}}
          {::fx/type :emit-event :event {:type         :event/tool-execution-start
                                         :tool-call-id (:id first-tool)
                                         :tool-name    (:name first-tool)
                                         :args         (:arguments first-tool)}}
          {::fx/type :execute-tool :tool-call first-tool}]]
        [(-> state
             (update :messages conj message)
             (assoc :node :node/idle)
             (assoc :stream-message nil))
         [{::fx/type :emit-event :event {:type :event/message-end :message message}}
          {::fx/type :emit-event :event {:type :event/turn-end :message message}}]]))

    :signal/llm-error
    [(-> state
         (assoc :node :node/idle)
         (assoc :error (:error msg)))
     [{::fx/type :emit-event :event {:type :event/message-end :message {:stop-reason :error}}}
      {::fx/type :emit-event :event {:type :event/turn-end}}
      {::fx/type :emit-event :event {:type :event/agent-end :messages (:messages state)}}]]

    :signal/abort
    [(-> state
         (assoc :node :node/closed)
         (assoc :stream-message nil))
     [{::fx/type :emit-event :event {:type :event/message-end :message {:stop-reason :aborted}}}
      {::fx/type :emit-event :event {:type :event/turn-end}}
      {::fx/type :emit-event :event {:type :event/agent-end :messages (:messages state)}}]]

    [state []]))

(defn tool-executing-transition
  "Pure transition from :node/tool-executing state. Returns [state' effects]."
  [state msg]
  (case (:type msg)
    :signal/tool-result
    (let [result      (:result msg)
          tool-result (:tool-result-message msg)
          remaining   (rest (:pending-tool-calls state))
          next-tool   (first remaining)
          state'      (-> state
                          (update :messages conj result)
                          (assoc :pending-tool-calls (vec remaining)))]
      (if (seq remaining)
        [state'
         [{::fx/type :emit-event :event {:type :event/tool-execution-end :result result}}
          {::fx/type :emit-event :event {:type :event/message-start :message tool-result}}
          {::fx/type :emit-event :event {:type :event/message-end :message tool-result}}
          {::fx/type :emit-event :event {:type         :event/tool-execution-start
                                         :tool-call-id (:id next-tool)
                                         :tool-name    (:name next-tool)
                                         :args         (:arguments next-tool)}}
          {::fx/type :execute-tool :tool-call next-tool}]]
        [state'
         [{::fx/type :emit-event :event {:type :event/tool-execution-end :result result}}
          {::fx/type :emit-event :event {:type :event/message-start :message tool-result}}
          {::fx/type :emit-event :event {:type :event/message-end :message tool-result}}
          {::fx/type :emit-event :event {:type :event/turn-end}}]]))

    :signal/tool-error
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
         [{::fx/type :emit-event :event {:type :event/tool-execution-end :result error-result}}
          {::fx/type :emit-event :event {:type :event/message-start :message tool-result}}
          {::fx/type :emit-event :event {:type :event/message-end :message tool-result}}
          {::fx/type :emit-event :event {:type         :event/tool-execution-start
                                         :tool-call-id (:id next-tool)
                                         :tool-name    (:name next-tool)
                                         :args         (:arguments next-tool)}}
          {::fx/type :execute-tool :tool-call next-tool}]]
        [state'
         [{::fx/type :emit-event :event {:type :event/tool-execution-end :result error-result}}
          {::fx/type :emit-event :event {:type :event/message-start :message tool-result}}
          {::fx/type :emit-event :event {:type :event/message-end :message tool-result}}
          {::fx/type :emit-event :event {:type :event/turn-end}}]]))

    :signal/tool-update
    [state
     [{::fx/type :emit-event
       :event    {:type           :event/tool-execution-update
                  :tool-call-id   (:tool-call-id msg)
                  :tool-name      (:tool-name msg)
                  :partial-result (:partial-result msg)}}]]

    :signal/abort
    [(-> state
         (assoc :node :node/closed)
         (assoc :pending-tool-calls []))
     [{::fx/type :emit-event :event {:type :event/turn-end}}
      {::fx/type :emit-event :event {:type :event/agent-end :messages (:messages state)}}]]

    [state []]))

(defn closed-transition
  "Terminal state. No transitions possible."
  [state _msg]
  [state []])

(defn route-from-idle [state]
  (:node state))

(defn route-from-streaming [state]
  (case (:node state)
    :node/idle (if (or (seq (:steering-queue state))
                       (seq (:follow-up-queue state)))
                 :node/streaming
                 :node/idle)
    :node/tool-executing :node/tool-executing
    :node/closed :node/closed
    :node/streaming))

(defn route-from-tool-executing [state]
  (case (:node state)
    :node/closed :node/closed
    (if (seq (:pending-tool-calls state))
      :node/tool-executing
      :node/streaming)))

(def graph
  "Agent state machine as data.

   Nodes: `(state, msg) -> [state', effects]`
     Pure transition fns. Take current state and a signal, return
     new state and a vector of effect descriptions to execute.

   Edges: `(state') -> node-key`
     Pure routing fns. Take the post-transition state and return
     the keyword of the next node to enter."
  {:nodes {:node/idle           idle-transition
           :node/streaming      streaming-transition
           :node/tool-executing tool-executing-transition
           :node/closed         closed-transition}

   :edges {:node/idle           route-from-idle
           :node/streaming      route-from-streaming
           :node/tool-executing route-from-tool-executing
           :node/closed         (constantly :node/closed)}})

(defn command?
  "Returns true if `input` is a command."
  [input]
  (= "command" (namespace (:type input))))

(defn- step-signal
  "Process a single signal through the TEA graph. Pure.
   Runs the transition fn for the current node, then the routing fn
   to determine the next node. Returns `[state' effects]`."
  [g state sig]
  (let [current-node     (:node state)
        transition-fn    (get-in g [:nodes current-node])
        routing-fn       (get-in g [:edges current-node])
        [state' effects] (transition-fn state sig)
        next-node        (routing-fn state')
        prev-node        current-node
        state''          (assoc state' :node next-node)
        agent-ended?     (and (not= :node/idle prev-node)
                              (= :node/idle next-node))]
    (if agent-ended?
      [state'' (conj effects {::fx/type :emit-event
                              :event    {:type     :event/agent-end
                                         :messages (:messages state'')}})]
      [state'' effects])))

(defn step
  "Pure TEA step function. Takes state + input (command or signal),
   returns `[state' effects]`.

   Commands are processed by `handle-command` which may produce signals.
   Signals are processed by the graph transition + routing fns.

   Emits `:event/agent-end` automatically when the agent transitions
   from a non-idle node back to `:node/idle`."
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
