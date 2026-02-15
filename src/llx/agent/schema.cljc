(ns llx.agent.schema
  (:require
   [com.fulcrologic.guardrails.malli.registry :as gr.reg]
   [llx.ai :as ai]
   [llx.ai.impl.schema :as ai.schema]
   [malli.core :as m]
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

     :llx.agent/command
     [:map
      [:type :keyword]]

     :llx.agent/create-agent-opts
     [:map
      [:convert-to-llm {:optional true} :llx/fn]
      [:transform-context {:optional true} :llx/fn]
      [:stream-fn {:optional true} :llx/fn]
      [:tool-defs :llx.agent/tools]
      [:schema-registry {:optional true} :llx.agent/schema-registry]
      [:custom-message-schemas {:optional true} :llx.agent/custom-message-schemas]
      [:session-id {:optional true} :llx/id-string]
      [:get-api-key {:optional true} :llx/fn]
      [:thinking-budgets {:optional true} :llx.agent/thinking-budgets]
      [:max-retry-delay-ms {:optional true} :llx/non-neg-int]
      [:system-prompt {:optional true} :string]
      [:model {:optional true} :llx/model]
      [:thinking-level {:optional true} :llx.agent.loop/thinking-level]
      [:tools {:optional true} [:vector :map]]
      [:steering-mode {:optional true} :llx.agent.loop/dequeue-mode]
      [:follow-up-mode {:optional true} :llx.agent.loop/dequeue-mode]
      [:abort-signal {:optional true} :any]]

     :llx.agent/tool-def
     [:map
      [:execute :llx/fn]]

     :llx.agent/tools
     [:map-of :string :llx.agent/tool-def]

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
      [:tools :llx.agent/tools]
      [:abort-signal {:optional true} :any]]

     :llx.agent.loop/node
     [:enum :node/idle :node/streaming :node/tool-executing :node/closed]

     :llx.agent.loop/thinking-level
     [:enum :off :minimal :low :medium :high :xhigh]

     :llx.agent.loop/dequeue-mode
     [:enum :one-at-a-time :all]

     :llx.agent.loop/state
     [:map
      [:node {:default :node/idle} :llx.agent.loop/node]
      [:system-prompt {:default ""} :string]
      [:model {:default (ai/get-model :openai "gpt-5.2-codex")} :llx/model]
      [:thinking-level {:default :off} :llx.agent.loop/thinking-level]
      [:tools {:default []} [:vector :map]]
      [:messages {:default []} :llx.agent/messages]
      [:stream-message {:default nil} [:maybe :llx.agent/message]]
      [:pending-tool-calls {:default []} [:vector :map]]
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
         (custom-schemas opts)))

(gr.reg/merge-schemas! (custom-schemas {}))

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
