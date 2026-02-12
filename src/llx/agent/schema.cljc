(ns llx.agent.schema
  (:require
   [com.fulcrologic.guardrails.malli.registry :as gr.reg]
   [llx.ai :as ai]
   [malli.core :as m]
   [malli.error :as me]
   [malli.util :as mu]))

(def canonical-message-roles #{:user :assistant :tool-result})

(defn schema-form?
  [x]
  (or (m/schema? x)
      (keyword? x)
      (vector? x)
      (map? x)
      (set? x)))

(defn custom-message-role?
  [role]
  (and (keyword? role)
       (not (contains? canonical-message-roles role))))

(defn message-dispatch
  [message]
  (let [role (:role message)]
    (if (contains? canonical-message-roles role)
      role
      :custom)))

(def ^:private llx-agent-merge-placeholder-registry
  {:llx.agent/state          :any
   :llx.agent/thinking-level :any
   :llx.agent/tool           :any
   :llx.agent/message        :any
   :llx.agent/stream-fn      :any
   :llx.agent/queue-mode     :any})

(defn- merge-with-unified-request-options
  [schema-form]
  (let [registry  (merge (ai/schema-regsitry)
                         llx-agent-merge-placeholder-registry)
        unified   (m/schema :llx/unified-request-options {:registry registry})
        extension (m/schema schema-form {:registry registry})]
    (m/form (mu/merge unified extension))))

(def schemas
  {:llx.agent/thinking-level
   [:enum :off :minimal :low :medium :high :xhigh]

   :llx.agent/thinking-budgets
   [:map-of :llx/reasoning-level :llx/non-neg-int]

   :llx.agent/stream-fn
   [:fn fn?]

   :llx.agent/tool-result
   [:map
    [:content [:and [:vector :llx/tool-result-content-block] [:fn seq]]]
    [:details :any]]

   :llx.agent/tool
   [:map
    [:name :llx/id-string]
    [:label :llx/id-string]
    [:description :llx/id-string]
    [:input-schema [:fn schema-form?]]
    [:execute [:fn fn?]]]

   :llx.agent/message-custom
   [:map
    [:role [:fn custom-message-role?]]]

   :llx.agent/message
   [:multi {:dispatch message-dispatch}
    [:user :llx/message-user]
    [:assistant :llx/message-assistant]
    [:tool-result :llx/message-tool-result]
    [:custom :llx.agent/message-custom]]

   :llx.agent/context
   [:map
    [:system-prompt :string]
    [:messages [:vector :llx.agent/message]]
    [:tools {:optional true} [:vector :llx.agent/tool]]]

   :llx.agent/state
   [:map
    [:system-prompt :string]
    [:model :llx/model]
    [:thinking-level :llx.agent/thinking-level]
    [:tools [:vector :llx.agent/tool]]
    [:messages [:vector :llx.agent/message]]
    [:streaming? :boolean]
    [:stream-message [:maybe :llx.agent/message]]
    [:pending-tool-calls [:set :llx/id-string]]
    [:error [:maybe :string]]]

   :llx.agent/loop-config
   (merge-with-unified-request-options
    [:map
     [:model :llx/model]
     [:convert-to-llm [:fn fn?]]
     [:transform-context {:optional true} [:fn fn?]]
     [:get-api-key {:optional true} [:fn fn?]]
     [:get-steering-messages {:optional true} [:fn fn?]]
     [:get-follow-up-messages {:optional true} [:fn fn?]]])

   :llx.agent/event-agent-start
   [:map
    [:llx.agent.event/type [:= :agent-start]]]

   :llx.agent/event-agent-end
   [:map
    [:llx.agent.event/type [:= :agent-end]]
    [:llx.agent.event/messages [:vector :llx.agent/message]]]

   :llx.agent/event-turn-start
   [:map
    [:llx.agent.event/type [:= :turn-start]]]

   :llx.agent/event-turn-end
   [:map
    [:llx.agent.event/type [:= :turn-end]]
    [:llx.agent.event/message :llx.agent/message]
    [:llx.agent.event/tool-results [:vector :llx/message-tool-result]]]

   :llx.agent/event-message-start
   [:map
    [:llx.agent.event/type [:= :message-start]]
    [:llx.agent.event/message :llx.agent/message]]

   :llx.agent/event-message-update
   [:map
    [:llx.agent.event/type [:= :message-update]]
    [:llx.agent.event/message :llx.agent/message]
    [:llx.agent.event/assistant-message-event :llx/event]]

   :llx.agent/event-message-end
   [:map
    [:llx.agent.event/type [:= :message-end]]
    [:llx.agent.event/message :llx.agent/message]]

   :llx.agent/event-tool-execution-start
   [:map
    [:llx.agent.event/type [:= :tool-execution-start]]
    [:llx.agent.event/tool-call-id :llx/id-string]
    [:llx.agent.event/tool-name :llx/id-string]
    [:llx.agent.event/args :map]]

   :llx.agent/event-tool-execution-update
   [:map
    [:llx.agent.event/type [:= :tool-execution-update]]
    [:llx.agent.event/tool-call-id :llx/id-string]
    [:llx.agent.event/tool-name :llx/id-string]
    [:llx.agent.event/args :map]
    [:llx.agent.event/partial-result :llx.agent/tool-result]]

   :llx.agent/event-tool-execution-end
   [:map
    [:llx.agent.event/type [:= :tool-execution-end]]
    [:llx.agent.event/tool-call-id :llx/id-string]
    [:llx.agent.event/tool-name :llx/id-string]
    [:llx.agent.event/result :llx.agent/tool-result]
    [:llx.agent.event/error? :boolean]]

   :llx.agent/event
   [:multi {:dispatch :llx.agent.event/type}
    [:agent-start :llx.agent/event-agent-start]
    [:agent-end :llx.agent/event-agent-end]
    [:turn-start :llx.agent/event-turn-start]
    [:turn-end :llx.agent/event-turn-end]
    [:message-start :llx.agent/event-message-start]
    [:message-update :llx.agent/event-message-update]
    [:message-end :llx.agent/event-message-end]
    [:tool-execution-start :llx.agent/event-tool-execution-start]
    [:tool-execution-update :llx.agent/event-tool-execution-update]
    [:tool-execution-end :llx.agent/event-tool-execution-end]]

   :llx.agent/proxy-event-start
   [:map
    [:llx.agent.proxy-event/type [:= :start]]]

   :llx.agent/proxy-event-text-start
   [:map
    [:llx.agent.proxy-event/type [:= :text-start]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]]

   :llx.agent/proxy-event-text-delta
   [:map
    [:llx.agent.proxy-event/type [:= :text-delta]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]
    [:llx.agent.proxy-event/delta :string]]

   :llx.agent/proxy-event-text-end
   [:map
    [:llx.agent.proxy-event/type [:= :text-end]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]
    [:llx.agent.proxy-event/content-signature {:optional true} :string]]

   :llx.agent/proxy-event-thinking-start
   [:map
    [:llx.agent.proxy-event/type [:= :thinking-start]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]]

   :llx.agent/proxy-event-thinking-delta
   [:map
    [:llx.agent.proxy-event/type [:= :thinking-delta]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]
    [:llx.agent.proxy-event/delta :string]]

   :llx.agent/proxy-event-thinking-end
   [:map
    [:llx.agent.proxy-event/type [:= :thinking-end]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]
    [:llx.agent.proxy-event/content-signature {:optional true} :string]]

   :llx.agent/proxy-event-toolcall-start
   [:map
    [:llx.agent.proxy-event/type [:= :toolcall-start]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]
    [:llx.agent.proxy-event/id :llx/id-string]
    [:llx.agent.proxy-event/tool-name :llx/id-string]]

   :llx.agent/proxy-event-toolcall-delta
   [:map
    [:llx.agent.proxy-event/type [:= :toolcall-delta]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]
    [:llx.agent.proxy-event/delta :string]]

   :llx.agent/proxy-event-toolcall-end
   [:map
    [:llx.agent.proxy-event/type [:= :toolcall-end]]
    [:llx.agent.proxy-event/content-index :llx/non-neg-int]]

   :llx.agent/proxy-event-done
   [:map
    [:llx.agent.proxy-event/type [:= :done]]
    [:llx.agent.proxy-event/reason :llx/stop-reason]
    [:llx.agent.proxy-event/usage :llx/usage]]

   :llx.agent/proxy-event-error
   [:map
    [:llx.agent.proxy-event/type [:= :error]]
    [:llx.agent.proxy-event/reason :llx/stop-reason]
    [:llx.agent.proxy-event/error-message {:optional true} :string]
    [:llx.agent.proxy-event/usage :llx/usage]]

   :llx.agent/proxy-event
   [:multi {:dispatch :llx.agent.proxy-event/type}
    [:start :llx.agent/proxy-event-start]
    [:text-start :llx.agent/proxy-event-text-start]
    [:text-delta :llx.agent/proxy-event-text-delta]
    [:text-end :llx.agent/proxy-event-text-end]
    [:thinking-start :llx.agent/proxy-event-thinking-start]
    [:thinking-delta :llx.agent/proxy-event-thinking-delta]
    [:thinking-end :llx.agent/proxy-event-thinking-end]
    [:toolcall-start :llx.agent/proxy-event-toolcall-start]
    [:toolcall-delta :llx.agent/proxy-event-toolcall-delta]
    [:toolcall-end :llx.agent/proxy-event-toolcall-end]
    [:done :llx.agent/proxy-event-done]
    [:error :llx.agent/proxy-event-error]]

   :llx.agent/proxy-options
   (merge-with-unified-request-options
    [:map
     [:proxy-url :llx/id-string]
     [:auth-token {:optional true} :llx/id-string]])

   :llx.agent/queue-mode
   [:enum :all :one-at-a-time]

   :llx.agent/agent-options
   (merge-with-unified-request-options
    [:map
     [:initial-state {:optional true} :llx.agent/state]
     [:model {:optional true} :llx/model]
     [:system-prompt {:optional true} :string]
     [:thinking-level {:optional true} :llx.agent/thinking-level]
     [:tools {:optional true} [:vector :llx.agent/tool]]
     [:messages {:optional true} [:vector :llx.agent/message]]
     [:convert-to-llm {:optional true} [:fn fn?]]
     [:transform-context {:optional true} [:fn fn?]]
     [:stream-fn {:optional true} :llx.agent/stream-fn]
     [:steering-mode {:optional true} :llx.agent/queue-mode]
     [:follow-up-mode {:optional true} :llx.agent/queue-mode]
     [:get-api-key {:optional true} [:fn fn?]]])})

(defn custom-schemas
  []
  schemas)

(defn registry
  []
  (merge (ai/schema-regsitry)
         (custom-schemas)))

(gr.reg/merge-schemas! (custom-schemas))

(defn schema
  [schema-id]
  (m/schema schema-id {:registry (registry)}))

(defn valid?
  [schema-id data]
  (m/validate (schema schema-id) data))

(defn explain
  [schema-id data]
  (m/explain (schema schema-id) data))

(defn humanize
  [schema-id data]
  (me/humanize (explain schema-id data)))

(defn validate!
  [schema-id data]
  (if (valid? schema-id data)
    data
    (throw
     (ex-info "Schema validation failed"
              {:schema schema-id
               :errors (humanize schema-id data)
               :data   data}))))
