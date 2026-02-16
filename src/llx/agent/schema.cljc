(ns llx.agent.schema
  (:require
   [com.fulcrologic.guardrails.malli.registry :as gr.reg]
   [llx.ai :as ai]
   [llx.ai.impl.schema :as ai.schema]
   [malli.core :as m]
   [malli.util :as mu]
   [malli.error :as me]
   [malli.registry :as mr]
   [promesa.protocols :as pt]
   [promesa.exec.csp :as sp]))

(defn state-atom?
  [x]
  #?(:clj (instance? clojure.lang.IAtom x)
     :cljs (satisfies? IAtom x)))

(defn queue? [coll]
  #? (:clj (instance? clojure.lang.PersistentQueue coll)
      :cljs
      (= (type coll) (type #queue []))))

(defn multiplexer?
  [x]
  (satisfies? pt/IChannelMultiplexer x))

(defn schema-registry?
  [x]
  (some? (mr/registry x)))

(def canonical-message-roles #{:user :assistant :tool-result})

(defn custom-message-role?
  [role]
  (and (qualified-keyword? role)
       (not (contains? canonical-message-roles role))))

(defn message-dispatch
  [message]
  (:role message))

(def ^:private canonical-message-schema-entries
  [[:user :llx/message-user]
   [:assistant :llx/message-assistant]
   [:tool-result :llx/message-tool-result]])

(defn schemas
  [{:keys [custom-message-schemas]}]
  (let [custom-message-schemas (or custom-message-schemas {})
        custom-message-entries (mapv (fn [[dispatch-keyword schema-name-keyword]]
                                       [dispatch-keyword schema-name-keyword])
                                     custom-message-schemas)]
    {:llx.agent/state-atom
     [:fn state-atom?]

     :llx.agent/channel
     [:fn sp/chan?]

     :llx.agent/multiplexer
     [:fn multiplexer?]

     :llx.agent/schema-registry
     [:fn schema-registry?]

     :llx.agent/custom-message-schemas
     [:map-of [:fn custom-message-role?] :keyword]

     :llx.agent/thinking-budgets
     [:map
      [:minimal {:optional true} :llx/non-neg-int]
      [:low {:optional true} :llx/non-neg-int]
      [:medium {:optional true} :llx/non-neg-int]
      [:high {:optional true} :llx/non-neg-int]]

     :llx.agent/message
     (into
      [:multi {:dispatch message-dispatch}]
      (concat canonical-message-schema-entries
              custom-message-entries))

     :llx.agent/messages
     [:vector :llx.agent/message]

     :llx.agent/tool-call
     [:map
      [:id :llx/id-string]
      [:name :llx/id-string]
      [:arguments :map]]

     :llx.agent/partial-assistant-message
     [:map
      [:role [:= :assistant]]
      [:content [:vector :llx/assistant-content-block]]
      [:api {:optional true} :llx/api]
      [:provider {:optional true} :llx/provider]
      [:model {:optional true} :llx/id-string]
      [:usage {:optional true} :llx/usage]
      [:stop-reason {:optional true} :llx/stop-reason]
      [:error-message {:optional true} :string]
      [:timestamp {:optional true} :llx/timestamp-ms]]

     :llx.agent/event-stop-message
     [:map
      [:stop-reason :llx/stop-reason]]

     :llx.agent/command-type
     [:enum
      :llx.agent.command/prompt ;; start inference with new messages
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
      ]

     :llx.agent/command-prompt
     [:map
      [:type [:= :llx.agent.command/prompt]]
      [:messages :llx.agent/messages]]

     :llx.agent/command-continue
     [:map
      [:type [:= :llx.agent.command/continue]]]

     :llx.agent/command-abort
     [:map
      [:type [:= :llx.agent.command/abort]]]

     :llx.agent/command-steer
     [:map
      [:type [:= :llx.agent.command/steer]]
      [:message :llx.agent/message]]

     :llx.agent/command-follow-up
     [:map
      [:type [:= :llx.agent.command/follow-up]]
      [:message :llx.agent/message]]

     :llx.agent/command-set-system-prompt
     [:map
      [:type [:= :llx.agent.command/set-system-prompt]]
      [:system-prompt :string]]

     :llx.agent/command-set-model
     [:map
      [:type [:= :llx.agent.command/set-model]]
      [:model :llx/model]]

     :llx.agent/command-set-thinking-level
     [:map
      [:type [:= :llx.agent.command/set-thinking-level]]
      [:thinking-level :llx.agent.loop/thinking-level]]

     :llx.agent/command-set-tools
     [:map
      [:type [:= :llx.agent.command/set-tools]]
      [:tools :llx.agent/tools]]

     :llx.agent/command-set-steering-mode
     [:map
      [:type [:= :llx.agent.command/set-steering-mode]]
      [:mode :llx.agent.loop/dequeue-mode]]

     :llx.agent/command-set-follow-up-mode
     [:map
      [:type [:= :llx.agent.command/set-follow-up-mode]]
      [:mode :llx.agent.loop/dequeue-mode]]

     :llx.agent/command-replace-messages
     [:map
      [:type [:= :llx.agent.command/replace-messages]]
      [:messages :llx.agent/messages]]

     :llx.agent/command-append-message
     [:map
      [:type [:= :llx.agent.command/append-message]]
      [:message :llx.agent/message]]

     :llx.agent/command-clear-messages
     [:map
      [:type [:= :llx.agent.command/clear-messages]]]

     :llx.agent/command-clear-steering-queue
     [:map
      [:type [:= :llx.agent.command/clear-steering-queue]]]

     :llx.agent/command-clear-follow-up-queue
     [:map
      [:type [:= :llx.agent.command/clear-follow-up-queue]]]

     :llx.agent/command-clear-all-queues
     [:map
      [:type [:= :llx.agent.command/clear-all-queues]]]

     :llx.agent/command-reset
     [:map
      [:type [:= :llx.agent.command/reset]]]

     :llx.agent/command
     [:multi {:dispatch :type}
      [:llx.agent.command/prompt :llx.agent/command-prompt]
      [:llx.agent.command/continue :llx.agent/command-continue]
      [:llx.agent.command/abort :llx.agent/command-abort]
      [:llx.agent.command/steer :llx.agent/command-steer]
      [:llx.agent.command/follow-up :llx.agent/command-follow-up]
      [:llx.agent.command/set-system-prompt :llx.agent/command-set-system-prompt]
      [:llx.agent.command/set-model :llx.agent/command-set-model]
      [:llx.agent.command/set-thinking-level :llx.agent/command-set-thinking-level]
      [:llx.agent.command/set-tools :llx.agent/command-set-tools]
      [:llx.agent.command/set-steering-mode :llx.agent/command-set-steering-mode]
      [:llx.agent.command/set-follow-up-mode :llx.agent/command-set-follow-up-mode]
      [:llx.agent.command/replace-messages :llx.agent/command-replace-messages]
      [:llx.agent.command/append-message :llx.agent/command-append-message]
      [:llx.agent.command/clear-messages :llx.agent/command-clear-messages]
      [:llx.agent.command/clear-steering-queue :llx.agent/command-clear-steering-queue]
      [:llx.agent.command/clear-follow-up-queue :llx.agent/command-clear-follow-up-queue]
      [:llx.agent.command/clear-all-queues :llx.agent/command-clear-all-queues]
      [:llx.agent.command/reset :llx.agent/command-reset]]

     :llx.agent/signal-type
     [:enum
      :llx.agent.signal/prompt-start ;; command/prompt accepted, begin inference
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
      ]

     :llx.agent/signal-prompt-start
     [:map
      [:type [:= :llx.agent.signal/prompt-start]]
      [:messages :llx.agent/messages]]

     :llx.agent/signal-continue-start
     [:map
      [:type [:= :llx.agent.signal/continue-start]]
      [:messages :llx.agent/messages]]

     :llx.agent/signal-abort
     [:map
      [:type [:= :llx.agent.signal/abort]]]

     :llx.agent/signal-rejected
     [:map
      [:type [:= :llx.agent.signal/rejected]]
      [:reason :keyword]]

     :llx.agent/signal-llm-start
     [:map
      [:type [:= :llx.agent.signal/llm-start]]
      [:message :llx.agent/partial-assistant-message]]

     :llx.agent/signal-llm-chunk
     [:map
      [:type [:= :llx.agent.signal/llm-chunk]]
      [:chunk :llx.agent/partial-assistant-message]]

     :llx.agent/signal-llm-done
     [:map
      [:type [:= :llx.agent.signal/llm-done]]
      [:message :llx/message-assistant]]

     :llx.agent/signal-llm-error
     [:map
      [:type [:= :llx.agent.signal/llm-error]]
      [:error :any]]

     :llx.agent/signal-tool-result
     [:map
      [:type [:= :llx.agent.signal/tool-result]]
      [:result :llx/message-tool-result]
      [:tool-result-message :llx/message-tool-result]]

     :llx.agent/signal-tool-error
     [:map
      [:type [:= :llx.agent.signal/tool-error]]
      [:tool-call-id :llx/id-string]
      [:error :any]
      [:tool-result-message :llx/message-tool-result]]

     :llx.agent/signal-tool-update
     [:map
      [:type [:= :llx.agent.signal/tool-update]]
      [:tool-call-id :llx/id-string]
      [:tool-name :llx/id-string]
      [:partial-result :map]]

     :llx.agent/signal
     [:multi {:dispatch :type}
      [:llx.agent.signal/prompt-start :llx.agent/signal-prompt-start]
      [:llx.agent.signal/continue-start :llx.agent/signal-continue-start]
      [:llx.agent.signal/abort :llx.agent/signal-abort]
      [:llx.agent.signal/rejected :llx.agent/signal-rejected]
      [:llx.agent.signal/llm-start :llx.agent/signal-llm-start]
      [:llx.agent.signal/llm-chunk :llx.agent/signal-llm-chunk]
      [:llx.agent.signal/llm-done :llx.agent/signal-llm-done]
      [:llx.agent.signal/llm-error :llx.agent/signal-llm-error]
      [:llx.agent.signal/tool-result :llx.agent/signal-tool-result]
      [:llx.agent.signal/tool-error :llx.agent/signal-tool-error]
      [:llx.agent.signal/tool-update :llx.agent/signal-tool-update]]

     :llx.agent/signals
     [:vector :llx.agent/signal]

     :llx.agent/event-type
     [:enum
      :llx.agent.event/agent-start ;; agent begins processing
      :llx.agent.event/agent-end ;; agent completes with all new messages
      :llx.agent.event/turn-start ;; new turn begins (one LLM call + tool executions)
      :llx.agent.event/turn-end ;; turn completes with assistant message and tool results
      :llx.agent.event/message-start ;; any message begins (user, assistant, tool-result)
      :llx.agent.event/message-update ;; assistant streaming chunk update
      :llx.agent.event/message-end ;; message fully received/processed
      :llx.agent.event/tool-execution-start ;; tool begins executing
      :llx.agent.event/tool-execution-update ;; tool streams progress
      :llx.agent.event/tool-execution-end ;; tool execution finished (success or error)
      ]

     :llx.agent/event-message-payload
     [:or
      :llx.agent/message
      :llx.agent/partial-assistant-message
      :llx.agent/event-stop-message]

     :llx.agent/event-agent-start
     [:map
      [:type [:= :llx.agent.event/agent-start]]]

     :llx.agent/event-agent-end
     [:map
      [:type [:= :llx.agent.event/agent-end]]
      [:messages :llx.agent/messages]]

     :llx.agent/event-turn-start
     [:map
      [:type [:= :llx.agent.event/turn-start]]]

     :llx.agent/event-turn-end
     [:map
      [:type [:= :llx.agent.event/turn-end]]
      [:message {:optional true} :llx.agent/event-message-payload]]

     :llx.agent/event-message-start
     [:map
      [:type [:= :llx.agent.event/message-start]]
      [:message :llx.agent/event-message-payload]]

     :llx.agent/event-message-update
     [:map
      [:type [:= :llx.agent.event/message-update]]
      [:chunk :llx.agent/event-message-payload]]

     :llx.agent/event-message-end
     [:map
      [:type [:= :llx.agent.event/message-end]]
      [:message :llx.agent/event-message-payload]]

     :llx.agent/event-tool-execution-start
     [:map
      [:type [:= :llx.agent.event/tool-execution-start]]
      [:tool-call-id :llx/id-string]
      [:tool-name :llx/id-string]
      [:args :map]]

     :llx.agent/event-tool-execution-update
     [:map
      [:type [:= :llx.agent.event/tool-execution-update]]
      [:tool-call-id :llx/id-string]
      [:tool-name :llx/id-string]
      [:partial-result :map]]

     :llx.agent/event-tool-execution-end
     [:map
      [:type [:= :llx.agent.event/tool-execution-end]]
      [:tool-call-id :llx/id-string]
      [:tool-name :llx/id-string]
      [:result :llx/message-tool-result]
      [:is-error? :boolean]]

     :llx.agent/event
     [:multi {:dispatch :type}
      [:llx.agent.event/agent-start :llx.agent/event-agent-start]
      [:llx.agent.event/agent-end :llx.agent/event-agent-end]
      [:llx.agent.event/turn-start :llx.agent/event-turn-start]
      [:llx.agent.event/turn-end :llx.agent/event-turn-end]
      [:llx.agent.event/message-start :llx.agent/event-message-start]
      [:llx.agent.event/message-update :llx.agent/event-message-update]
      [:llx.agent.event/message-end :llx.agent/event-message-end]
      [:llx.agent.event/tool-execution-start :llx.agent/event-tool-execution-start]
      [:llx.agent.event/tool-execution-update :llx.agent/event-tool-execution-update]
      [:llx.agent.event/tool-execution-end :llx.agent/event-tool-execution-end]]

     :llx.agent.fx/effect-emit-event
     [:map
      [:llx.agent.fx/type [:= :emit-event]]
      [:event :llx.agent/event]]

     :llx.agent.fx/effect-call-llm
     [:map
      [:llx.agent.fx/type [:= :call-llm]]
      [:messages :llx.agent/messages]]

     :llx.agent.fx/effect-execute-tool
     [:map
      [:llx.agent.fx/type [:= :execute-tool]]
      [:tool-call :llx.agent/tool-call]]

     :llx.agent.fx/effect-reject
     [:map
      [:llx.agent.fx/type [:= :reject]]
      [:reason :keyword]]

     :llx.agent.fx/effect
     [:multi {:dispatch :llx.agent.fx/type}
      [:emit-event :llx.agent.fx/effect-emit-event]
      [:call-llm :llx.agent.fx/effect-call-llm]
      [:execute-tool :llx.agent.fx/effect-execute-tool]
      [:reject :llx.agent.fx/effect-reject]]

     :llx.agent.fx/result
     [:or :llx.agent/channel :nil]

     :llx.agent/create-agent-opts
     [:map
      [:convert-to-llm {:optional true} :llx/fn]
      [:transform-context {:optional true} :llx/fn]
      [:stream-fn {:optional true} :llx/fn]
      [:tools :llx.agent/tools]
      [:schema-registry {:optional true} :llx.agent/schema-registry]
      [:custom-message-schemas {:optional true} :llx.agent/custom-message-schemas]
      [:session-id {:optional true} :llx/id-string]
      [:get-api-key {:optional true} :llx/fn]
      [:thinking-budgets {:optional true} :llx.agent/thinking-budgets]
      [:max-retry-delay-ms {:optional true} :llx/non-neg-int]
      [:system-prompt {:optional true} :string]
      [:model {:optional true} :llx/model]
      [:thinking-level {:optional true} :llx.agent.loop/thinking-level]
      [:steering-mode {:optional true} :llx.agent.loop/dequeue-mode]
      [:follow-up-mode {:optional true} :llx.agent.loop/dequeue-mode]
      [:abort-signal {:optional true} :any]]

     :llx.agent/tool
     [:merge
      :llx/tool
      [:map  [:execute :llx/fn]]]

     :llx.agent/tools
     [:vector :llx.agent/tool]

     :llx.agent/env
     [:map
      [:state_ :llx.agent/state-atom]
      [:command> :llx.agent/channel]
      [:events-mx> :llx.agent/multiplexer]
      [:schema-registry :llx.agent/schema-registry]
      [:convert-to-llm :llx/fn]
      [:transform-context {:optional true} [:maybe :llx/fn]]
      [:stream-fn {:optional true} [:maybe :llx/fn]]
      [:session-id {:optional true} :llx/id-string]
      [:get-api-key {:optional true} [:maybe :llx/fn]]
      [:thinking-budgets {:optional true} :llx.agent/thinking-budgets]
      [:max-retry-delay-ms {:optional true} :llx/non-neg-int]
      [:abort-signal {:optional true} :any]]

     :llx.agent.loop/phase
     [:enum
      :llx.agent.loop/idle ;; no inference loop is running, awaiting prompt, continue, or abort
      :llx.agent.loop/streaming ;; actively streaming an llm inference response
      :llx.agent.loop/tool-executing ;; a tool call is in progress
      :llx.agent.loop/closed ;; terminal state
      ]

     :llx.agent.loop/thinking-level
     [:enum :off :minimal :low :medium :high :xhigh]

     :llx.agent.loop/dequeue-mode
     [:enum :one-at-a-time :all]

     :llx.agent.loop/state
     [:map
      [:llx.agent.loop/phase {:default :llx.agent.loop/idle} :llx.agent.loop/phase]
      [:system-prompt {:default ""} :string]
      [:model {:default (ai/get-model :openai "gpt-5.2-codex")} :llx/model]
      [:thinking-level {:default :off} :llx.agent.loop/thinking-level]
      [:tools {:default []} :llx.agent/tools]
      [:messages {:default []} :llx.agent/messages]
      [:stream-message {:default nil} [:maybe :llx.agent/message]]
      [:pending-tool-calls {:default []} [:vector :llx.agent/tool-call]]
      [:error {:default nil} [:maybe :any]]
      [:steering-queue {:default #?(:clj clojure.lang.PersistentQueue/EMPTY
                                    :cljs #queue [])} [:fn queue?]]
      [:follow-up-queue {:default #?(:clj clojure.lang.PersistentQueue/EMPTY
                                     :cljs #queue [])} [:fn queue?]]
      [:steering-mode {:default :one-at-a-time} :llx.agent.loop/dequeue-mode]
      [:follow-up-mode {:default :one-at-a-time} :llx.agent.loop/dequeue-mode]]}))

(defn custom-schemas
  [opts]
  (merge (ai.schema/custom-schemas)
         (schemas opts)))

(defn registry
  [opts]
  (merge (m/default-schemas)
         (mu/schemas)
         (custom-schemas opts)))

(gr.reg/merge-schemas! (merge (mu/schemas)
                              (custom-schemas {})))

(defn validate!
  [schema-registry schema-id data]
  (let [schema (m/schema schema-id {:registry schema-registry})]
    (if (m/validate schema data)
      data
      (throw
       (ex-info "Schema validation failed"
                {:schema schema-id
                 :errors (me/humanize (m/explain schema data))
                 :data   data})))))
