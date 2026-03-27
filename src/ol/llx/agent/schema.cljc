(ns ol.llx.agent.schema
  (:require
   [com.fulcrologic.guardrails.malli.registry :as gr.reg]
   [ol.llx.ai :as ai]
   [ol.llx.ai.impl.schema :as ai.schema]
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
  [[:user :ol.llx/message-user]
   [:assistant :ol.llx/message-assistant]
   [:tool-result :ol.llx/message-tool-result]])

(defn schemas
  [{:keys [custom-message-schemas]}]
  (let [custom-message-schemas (or custom-message-schemas {})
        custom-message-entries (mapv (fn [[dispatch-keyword schema-name-keyword]]
                                       [dispatch-keyword schema-name-keyword])
                                     custom-message-schemas)]
    {:ol.llx.agent/state-atom
     [:fn state-atom?]

     :ol.llx.agent/channel
     [:fn sp/chan?]

     :ol.llx.agent/multiplexer
     [:fn multiplexer?]

     :ol.llx.agent/schema-registry
     [:fn schema-registry?]

     :ol.llx.agent/custom-message-schemas
     [:map-of [:fn custom-message-role?] :keyword]

     :ol.llx.agent/thinking-budgets
     [:map
      [:minimal {:optional true} :ol.llx/non-neg-int]
      [:low {:optional true} :ol.llx/non-neg-int]
      [:medium {:optional true} :ol.llx/non-neg-int]
      [:high {:optional true} :ol.llx/non-neg-int]]

     :ol.llx.agent/message
     (into
      [:multi {:dispatch message-dispatch}]
      (concat canonical-message-schema-entries
              custom-message-entries))

     :ol.llx.agent/messages
     [:vector :ol.llx.agent/message]

     :ol.llx.agent/tool-call
     [:map
      [:id :ol.llx/id-string]
      [:name :ol.llx/id-string]
      [:arguments :map]]

     :ol.llx.agent/partial-assistant-message
     [:map
      [:role [:= :assistant]]
      [:content [:vector :ol.llx/assistant-content-block]]
      [:api {:optional true} :ol.llx/api]
      [:provider {:optional true} :ol.llx/provider]
      [:model {:optional true} :ol.llx/id-string]
      [:usage {:optional true} :ol.llx/usage]
      [:stop-reason {:optional true} :ol.llx/stop-reason]
      [:error-message {:optional true} :string]
      [:timestamp {:optional true} :ol.llx/timestamp-ms]]

     :ol.llx.agent/run-id
     :ol.llx/non-neg-int

     :ol.llx.agent/event-stop-message
     [:map
      [:stop-reason :ol.llx/stop-reason]]

     :ol.llx.agent/command-type
     [:enum
      :ol.llx.agent.command/prompt ;; start inference with new messages
      :ol.llx.agent.command/continue ;; resume with queued steering or follow-up messages
      :ol.llx.agent.command/abort ;; cancel the running loop
      :ol.llx.agent.command/close ;; terminate the runtime
      :ol.llx.agent.command/steer ;; inject a steering message mid-run
      :ol.llx.agent.command/follow-up ;; queue a message for after the agent finishes
      :ol.llx.agent.command/set-system-prompt ;; replace the system prompt
      :ol.llx.agent.command/set-model ;; change the LLM model
      :ol.llx.agent.command/set-thinking-level ;; set reasoning level (off, low, medium, high, etc.)
      :ol.llx.agent.command/set-tools ;; replace the available tool set
      :ol.llx.agent.command/set-steering-mode ;; set steering dequeue mode (all or one-at-a-time)
      :ol.llx.agent.command/set-follow-up-mode ;; set follow-up dequeue mode (all or one-at-a-time)
      :ol.llx.agent.command/replace-messages ;; replace the entire message history
      :ol.llx.agent.command/append-message ;; append a single message to history
      :ol.llx.agent.command/clear-messages ;; clear the message history
      :ol.llx.agent.command/clear-steering-queue ;; drop all queued steering messages
      :ol.llx.agent.command/clear-follow-up-queue ;; drop all queued follow-up messages
      :ol.llx.agent.command/clear-all-queues ;; drop all queued steering and follow-up messages
      :ol.llx.agent.command/reset ;; clear all state (messages, queues, errors)
      ]

     :ol.llx.agent/command-prompt
     [:map
      [:type [:= :ol.llx.agent.command/prompt]]
      [:messages :ol.llx.agent/messages]]

     :ol.llx.agent/command-continue
     [:map
      [:type [:= :ol.llx.agent.command/continue]]]

     :ol.llx.agent/command-abort
     [:map
      [:type [:= :ol.llx.agent.command/abort]]]

     :ol.llx.agent/command-close
     [:map
      [:type [:= :ol.llx.agent.command/close]]]

     :ol.llx.agent/command-steer
     [:map
      [:type [:= :ol.llx.agent.command/steer]]
      [:message :ol.llx.agent/message]]

     :ol.llx.agent/command-follow-up
     [:map
      [:type [:= :ol.llx.agent.command/follow-up]]
      [:message :ol.llx.agent/message]]

     :ol.llx.agent/command-set-system-prompt
     [:map
      [:type [:= :ol.llx.agent.command/set-system-prompt]]
      [:system-prompt :string]]

     :ol.llx.agent/command-set-model
     [:map
      [:type [:= :ol.llx.agent.command/set-model]]
      [:model :ol.llx/model]]

     :ol.llx.agent/command-set-thinking-level
     [:map
      [:type [:= :ol.llx.agent.command/set-thinking-level]]
      [:thinking-level :ol.llx.agent.loop/thinking-level]]

     :ol.llx.agent/command-set-tools
     [:map
      [:type [:= :ol.llx.agent.command/set-tools]]
      [:tools :ol.llx.agent/tools]]

     :ol.llx.agent/command-set-steering-mode
     [:map
      [:type [:= :ol.llx.agent.command/set-steering-mode]]
      [:mode :ol.llx.agent.loop/dequeue-mode]]

     :ol.llx.agent/command-set-follow-up-mode
     [:map
      [:type [:= :ol.llx.agent.command/set-follow-up-mode]]
      [:mode :ol.llx.agent.loop/dequeue-mode]]

     :ol.llx.agent/command-replace-messages
     [:map
      [:type [:= :ol.llx.agent.command/replace-messages]]
      [:messages :ol.llx.agent/messages]]

     :ol.llx.agent/command-append-message
     [:map
      [:type [:= :ol.llx.agent.command/append-message]]
      [:message :ol.llx.agent/message]]

     :ol.llx.agent/command-clear-messages
     [:map
      [:type [:= :ol.llx.agent.command/clear-messages]]]

     :ol.llx.agent/command-clear-steering-queue
     [:map
      [:type [:= :ol.llx.agent.command/clear-steering-queue]]]

     :ol.llx.agent/command-clear-follow-up-queue
     [:map
      [:type [:= :ol.llx.agent.command/clear-follow-up-queue]]]

     :ol.llx.agent/command-clear-all-queues
     [:map
      [:type [:= :ol.llx.agent.command/clear-all-queues]]]

     :ol.llx.agent/command-reset
     [:map
      [:type [:= :ol.llx.agent.command/reset]]]

     :ol.llx.agent/command
     [:multi {:dispatch :type}
      [:ol.llx.agent.command/prompt :ol.llx.agent/command-prompt]
      [:ol.llx.agent.command/continue :ol.llx.agent/command-continue]
      [:ol.llx.agent.command/abort :ol.llx.agent/command-abort]
      [:ol.llx.agent.command/close :ol.llx.agent/command-close]
      [:ol.llx.agent.command/steer :ol.llx.agent/command-steer]
      [:ol.llx.agent.command/follow-up :ol.llx.agent/command-follow-up]
      [:ol.llx.agent.command/set-system-prompt :ol.llx.agent/command-set-system-prompt]
      [:ol.llx.agent.command/set-model :ol.llx.agent/command-set-model]
      [:ol.llx.agent.command/set-thinking-level :ol.llx.agent/command-set-thinking-level]
      [:ol.llx.agent.command/set-tools :ol.llx.agent/command-set-tools]
      [:ol.llx.agent.command/set-steering-mode :ol.llx.agent/command-set-steering-mode]
      [:ol.llx.agent.command/set-follow-up-mode :ol.llx.agent/command-set-follow-up-mode]
      [:ol.llx.agent.command/replace-messages :ol.llx.agent/command-replace-messages]
      [:ol.llx.agent.command/append-message :ol.llx.agent/command-append-message]
      [:ol.llx.agent.command/clear-messages :ol.llx.agent/command-clear-messages]
      [:ol.llx.agent.command/clear-steering-queue :ol.llx.agent/command-clear-steering-queue]
      [:ol.llx.agent.command/clear-follow-up-queue :ol.llx.agent/command-clear-follow-up-queue]
      [:ol.llx.agent.command/clear-all-queues :ol.llx.agent/command-clear-all-queues]
      [:ol.llx.agent.command/reset :ol.llx.agent/command-reset]]

     :ol.llx.agent/signal-type
     [:enum
      :ol.llx.agent.signal/prompt-start ;; command/prompt accepted, begin inference
      :ol.llx.agent.signal/continue-start ;; command/continue accepted with dequeued messages
      :ol.llx.agent.signal/abort ;; command/abort accepted, cancel running loop
      :ol.llx.agent.signal/rejected ;; command was rejected (carries :reason)
      :ol.llx.agent.signal/llm-start ;; LLM stream begun, initial partial message available
      :ol.llx.agent.signal/llm-chunk ;; streaming chunk received from LLM
      :ol.llx.agent.signal/llm-done ;; LLM inference completed
      :ol.llx.agent.signal/llm-error ;; LLM inference failed
      :ol.llx.agent.signal/tool-result ;; tool execution completed successfully
      :ol.llx.agent.signal/tool-error ;; tool execution failed
      :ol.llx.agent.signal/tool-update ;; tool execution progress update
      ]

     :ol.llx.agent/signal-prompt-start
     [:map
      [:type [:= :ol.llx.agent.signal/prompt-start]]
      [:messages :ol.llx.agent/messages]]

     :ol.llx.agent/signal-continue-start
     [:map
      [:type [:= :ol.llx.agent.signal/continue-start]]
      [:messages :ol.llx.agent/messages]]

     :ol.llx.agent/signal-abort
     [:map
      [:type [:= :ol.llx.agent.signal/abort]]]

     :ol.llx.agent/signal-rejected
     [:map
      [:type [:= :ol.llx.agent.signal/rejected]]
      [:reason :keyword]]

     :ol.llx.agent/signal-llm-start
     [:map
      [:type [:= :ol.llx.agent.signal/llm-start]]
      [:run-id {:optional true} :ol.llx.agent/run-id]
      [:message :ol.llx.agent/partial-assistant-message]]

     :ol.llx.agent/signal-llm-chunk
     [:map
      [:type [:= :ol.llx.agent.signal/llm-chunk]]
      [:run-id {:optional true} :ol.llx.agent/run-id]
      [:chunk :ol.llx.agent/partial-assistant-message]]

     :ol.llx.agent/signal-llm-done
     [:map
      [:type [:= :ol.llx.agent.signal/llm-done]]
      [:run-id {:optional true} :ol.llx.agent/run-id]
      [:message :ol.llx/message-assistant]]

     :ol.llx.agent/signal-llm-error
     [:map
      [:type [:= :ol.llx.agent.signal/llm-error]]
      [:run-id {:optional true} :ol.llx.agent/run-id]
      [:error :any]]

     :ol.llx.agent/signal-tool-result
     [:map
      [:type [:= :ol.llx.agent.signal/tool-result]]
      [:run-id {:optional true} :ol.llx.agent/run-id]
      [:result :ol.llx/message-tool-result]
      [:tool-result-message :ol.llx/message-tool-result]]

     :ol.llx.agent/signal-tool-error
     [:map
      [:type [:= :ol.llx.agent.signal/tool-error]]
      [:run-id {:optional true} :ol.llx.agent/run-id]
      [:tool-call-id :ol.llx/id-string]
      [:error :any]
      [:tool-result-message :ol.llx/message-tool-result]]

     :ol.llx.agent/signal-tool-update
     [:map
      [:type [:= :ol.llx.agent.signal/tool-update]]
      [:run-id {:optional true} :ol.llx.agent/run-id]
      [:tool-call-id :ol.llx/id-string]
      [:tool-name :ol.llx/id-string]
      [:partial-result :map]]

     :ol.llx.agent/signal
     [:multi {:dispatch :type}
      [:ol.llx.agent.signal/prompt-start :ol.llx.agent/signal-prompt-start]
      [:ol.llx.agent.signal/continue-start :ol.llx.agent/signal-continue-start]
      [:ol.llx.agent.signal/abort :ol.llx.agent/signal-abort]
      [:ol.llx.agent.signal/rejected :ol.llx.agent/signal-rejected]
      [:ol.llx.agent.signal/llm-start :ol.llx.agent/signal-llm-start]
      [:ol.llx.agent.signal/llm-chunk :ol.llx.agent/signal-llm-chunk]
      [:ol.llx.agent.signal/llm-done :ol.llx.agent/signal-llm-done]
      [:ol.llx.agent.signal/llm-error :ol.llx.agent/signal-llm-error]
      [:ol.llx.agent.signal/tool-result :ol.llx.agent/signal-tool-result]
      [:ol.llx.agent.signal/tool-error :ol.llx.agent/signal-tool-error]
      [:ol.llx.agent.signal/tool-update :ol.llx.agent/signal-tool-update]]

     :ol.llx.agent/signals
     [:vector :ol.llx.agent/signal]

     :ol.llx.agent/event-type
     [:enum
      :ol.llx.agent.event/agent-start ;; agent begins processing
      :ol.llx.agent.event/agent-end ;; agent completes with all new messages
      :ol.llx.agent.event/turn-start ;; new turn begins (one LLM call + tool executions)
      :ol.llx.agent.event/turn-end ;; turn completes with assistant message and tool results
      :ol.llx.agent.event/message-start ;; any message begins (user, assistant, tool-result)
      :ol.llx.agent.event/message-update ;; assistant streaming chunk update
      :ol.llx.agent.event/message-end ;; message fully received/processed
      :ol.llx.agent.event/tool-execution-start ;; tool begins executing
      :ol.llx.agent.event/tool-execution-update ;; tool streams progress
      :ol.llx.agent.event/tool-execution-end ;; tool execution finished (success or error)
      ]

     :ol.llx.agent/event-message-payload
     [:or
      :ol.llx.agent/message
      :ol.llx.agent/partial-assistant-message
      :ol.llx.agent/event-stop-message]

     :ol.llx.agent/event-agent-start
     [:map
      [:type [:= :ol.llx.agent.event/agent-start]]]

     :ol.llx.agent/event-agent-end
     [:map
      [:type [:= :ol.llx.agent.event/agent-end]]
      [:messages :ol.llx.agent/messages]]

     :ol.llx.agent/event-turn-start
     [:map
      [:type [:= :ol.llx.agent.event/turn-start]]]

     :ol.llx.agent/event-turn-end
     [:map
      [:type [:= :ol.llx.agent.event/turn-end]]
      [:message {:optional true} :ol.llx.agent/event-message-payload]]

     :ol.llx.agent/event-message-start
     [:map
      [:type [:= :ol.llx.agent.event/message-start]]
      [:message :ol.llx.agent/event-message-payload]]

     :ol.llx.agent/event-message-update
     [:map
      [:type [:= :ol.llx.agent.event/message-update]]
      [:chunk :ol.llx.agent/event-message-payload]]

     :ol.llx.agent/event-message-end
     [:map
      [:type [:= :ol.llx.agent.event/message-end]]
      [:message :ol.llx.agent/event-message-payload]]

     :ol.llx.agent/event-tool-execution-start
     [:map
      [:type [:= :ol.llx.agent.event/tool-execution-start]]
      [:tool-call-id :ol.llx/id-string]
      [:tool-name :ol.llx/id-string]
      [:args :map]]

     :ol.llx.agent/event-tool-execution-update
     [:map
      [:type [:= :ol.llx.agent.event/tool-execution-update]]
      [:tool-call-id :ol.llx/id-string]
      [:tool-name :ol.llx/id-string]
      [:partial-result :map]]

     :ol.llx.agent/event-tool-execution-end
     [:map
      [:type [:= :ol.llx.agent.event/tool-execution-end]]
      [:tool-call-id :ol.llx/id-string]
      [:tool-name :ol.llx/id-string]
      [:result :ol.llx/message-tool-result]
      [:is-error? :boolean]]

     :ol.llx.agent/event
     [:multi {:dispatch :type}
      [:ol.llx.agent.event/agent-start :ol.llx.agent/event-agent-start]
      [:ol.llx.agent.event/agent-end :ol.llx.agent/event-agent-end]
      [:ol.llx.agent.event/turn-start :ol.llx.agent/event-turn-start]
      [:ol.llx.agent.event/turn-end :ol.llx.agent/event-turn-end]
      [:ol.llx.agent.event/message-start :ol.llx.agent/event-message-start]
      [:ol.llx.agent.event/message-update :ol.llx.agent/event-message-update]
      [:ol.llx.agent.event/message-end :ol.llx.agent/event-message-end]
      [:ol.llx.agent.event/tool-execution-start :ol.llx.agent/event-tool-execution-start]
      [:ol.llx.agent.event/tool-execution-update :ol.llx.agent/event-tool-execution-update]
      [:ol.llx.agent.event/tool-execution-end :ol.llx.agent/event-tool-execution-end]]

     :ol.llx.agent.fx/effect-emit-event
     [:map
      [:ol.llx.agent.fx/type [:= :emit-event]]
      [:event :ol.llx.agent/event]]

     :ol.llx.agent.fx/effect-call-llm
     [:map
      [:ol.llx.agent.fx/type [:= :call-llm]]
      [:messages :ol.llx.agent/messages]]

     :ol.llx.agent.fx/effect-execute-tool
     [:map
      [:ol.llx.agent.fx/type [:= :execute-tool]]
      [:tool-call :ol.llx.agent/tool-call]]

     :ol.llx.agent.fx/effect-reject
     [:map
      [:ol.llx.agent.fx/type [:= :reject]]
      [:reason :keyword]]

     :ol.llx.agent.fx/effect
     [:multi {:dispatch :ol.llx.agent.fx/type}
      [:emit-event :ol.llx.agent.fx/effect-emit-event]
      [:call-llm :ol.llx.agent.fx/effect-call-llm]
      [:execute-tool :ol.llx.agent.fx/effect-execute-tool]
      [:reject :ol.llx.agent.fx/effect-reject]]

     :ol.llx.agent.fx/result
     [:or :ol.llx.agent.fx/handle :nil]

     :ol.llx.agent.fx/handle
     [:map
      [:signals> :ol.llx.agent/channel]
      [:cancel! :ol.llx/fn]]

     :ol.llx.agent/create-agent-opts
     [:map
      [:convert-to-llm {:optional true} :ol.llx/fn]
      [:transform-context {:optional true} :ol.llx/fn]
      [:stream-fn {:optional true} :ol.llx/fn]
      [:tools :ol.llx.agent/tools]
      [:schema-registry {:optional true} :ol.llx.agent/schema-registry]
      [:custom-message-schemas {:optional true} :ol.llx.agent/custom-message-schemas]
      [:session-id {:optional true} :ol.llx/id-string]
      [:get-api-key {:optional true} :ol.llx/fn]
      [:thinking-budgets {:optional true} :ol.llx.agent/thinking-budgets]
      [:max-retry-delay-ms {:optional true} :ol.llx/non-neg-int]
      [:system-prompt {:optional true} :string]
      [:model {:optional true} :ol.llx/model]
      [:thinking-level {:optional true} :ol.llx.agent.loop/thinking-level]
      [:steering-mode {:optional true} :ol.llx.agent.loop/dequeue-mode]
      [:follow-up-mode {:optional true} :ol.llx.agent.loop/dequeue-mode]
      [:abort-signal {:optional true} :any]]

     :ol.llx.agent/tool
     [:merge
      :ol.llx/tool
      [:map  [:execute :ol.llx/fn]]]

     :ol.llx.agent/tools
     [:vector :ol.llx.agent/tool]

     :ol.llx.agent/env
     [:map
      [:state_ :ol.llx.agent/state-atom]
      [:command> :ol.llx.agent/channel]
      [:events-mx> :ol.llx.agent/multiplexer]
      [:schema-registry :ol.llx.agent/schema-registry]
      [:convert-to-llm :ol.llx/fn]
      [:transform-context {:optional true} [:maybe :ol.llx/fn]]
      [:stream-fn {:optional true} [:maybe :ol.llx/fn]]
      [:session-id {:optional true} :ol.llx/id-string]
      [:get-api-key {:optional true} [:maybe :ol.llx/fn]]
      [:thinking-budgets {:optional true} :ol.llx.agent/thinking-budgets]
      [:max-retry-delay-ms {:optional true} :ol.llx/non-neg-int]
      [:abort-signal {:optional true} :any]]

     :ol.llx.agent/runtime-active-run
     [:map
      [:id :ol.llx.agent/run-id]
      [:signal :any]
      [:cancel! :ol.llx/fn]
      [:cancelled? :ol.llx/fn]
      [:effect-handles [:map-of :ol.llx.agent/channel :ol.llx.agent.fx/handle]]
      [:reply {:optional true} [:maybe :ol.llx/deferred]]]

     :ol.llx.agent/runtime
     [:map
      [:status :keyword]
      [:closing? :boolean]
      [:next-run-id :ol.llx.agent/run-id]
      [:active-run [:maybe :ol.llx.agent/runtime-active-run]]]

     :ol.llx.agent/runtime-state
     [:map
      [:public-state :ol.llx.agent.loop/state]
      [:runtime :ol.llx.agent/runtime]]

     :ol.llx.agent.loop/phase
     [:enum
      :ol.llx.agent.loop/idle ;; no inference loop is running, awaiting prompt, continue, or abort
      :ol.llx.agent.loop/streaming ;; actively streaming an llm inference response
      :ol.llx.agent.loop/tool-executing ;; a tool call is in progress
      :ol.llx.agent.loop/closed ;; terminal state
      ]

     :ol.llx.agent.loop/thinking-level
     [:enum :off :minimal :low :medium :high :xhigh]

     :ol.llx.agent.loop/dequeue-mode
     [:enum :one-at-a-time :all]

     :ol.llx.agent.loop/state
     [:map
      [:ol.llx.agent.loop/phase {:default :ol.llx.agent.loop/idle} :ol.llx.agent.loop/phase]
      [:system-prompt {:default ""} :string]
      [:model {:default (ai/get-model :openai "gpt-5.2-codex")} :ol.llx/model]
      [:thinking-level {:default :off} :ol.llx.agent.loop/thinking-level]
      [:tools {:default []} :ol.llx.agent/tools]
      [:messages {:default []} :ol.llx.agent/messages]
      [:stream-message {:default nil} [:maybe :ol.llx.agent/message]]
      [:pending-tool-calls {:default []} [:vector :ol.llx.agent/tool-call]]
      [:error {:default nil} [:maybe :any]]
      [:steering-queue {:default #?(:clj clojure.lang.PersistentQueue/EMPTY
                                    :cljs #queue [])} [:fn queue?]]
      [:follow-up-queue {:default #?(:clj clojure.lang.PersistentQueue/EMPTY
                                     :cljs #queue [])} [:fn queue?]]
      [:steering-mode {:default :one-at-a-time} :ol.llx.agent.loop/dequeue-mode]
      [:follow-up-mode {:default :one-at-a-time} :ol.llx.agent.loop/dequeue-mode]]}))

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
