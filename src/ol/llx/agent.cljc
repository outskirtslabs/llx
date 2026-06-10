(ns ol.llx.agent
  "Public runtime wrapper for the agent loop.

   This namespace wires runtime state, effect environment dependencies,
   and subscription management around `ol.llx.agent.loop` + `ol.llx.agent.fx`."
  (:refer-clojure :exclude [reset!])
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [ol.llx.agent.driver :as driver]
   [ol.llx.agent.loop :as loop]
   [ol.llx.agent.schema :as schema]
   [malli.core :as m]
   [malli.transform :as mt]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(def ^:private default-subscription-buffer-size
  256)

(def ^:private default-transformer
  (mt/default-value-transformer {::mt/add-optional-keys true}))

(def ^:private base-schema-registry
  (schema/registry {}))

(defn- schema-config
  [m]
  (m/coerce :ol.llx.agent/schema-config
            (select-keys m [:schema-registry :custom-message-schemas])
            default-transformer
            {:registry base-schema-registry}))

(defn- live-schema-registry
  [public-state]
  (schema/derive-active-registry (schema-config public-state)))

(defn create-initial-state
  [schema-registry schema-config opts]
  (m/coerce :ol.llx.agent.loop/state
            (merge schema-config
                   (select-keys opts
                                [:system-prompt
                                 :model
                                 :thinking-level
                                 :tools
                                 :steering-mode
                                 :follow-up-mode]))
            default-transformer
            {:registry schema-registry}))

(>defn default-convert-to-llm
       "Converts agent messages to LLM-compatible messages by keeping only maps
   whose `:role` is one of `:user`, `:assistant`, or `:tool-result`."
       [messages]
       [:ol.llx.agent/messages => :ol.llx/messages]
       (->> messages
            (filter (fn [message]
                      (#{:user :assistant :tool-result} (:role message))))
            vec))

(defn- runtime-handle
  [state opts]
  (let [schema-registry (live-schema-registry state)]
    (schema/validate! schema-registry :ol.llx.agent.loop/state state)
    (let [command>     (sp/chan :buf (sp/sliding-buffer 64))
          events-mx>   (sp/mult :buf (sp/sliding-buffer default-subscription-buffer-size))
          optional-env (->> (select-keys opts [:session-id :get-api-key :thinking-budgets :max-retry-delay-ms])
                            (remove (comp nil? val))
                            (into {}))
          env          (m/coerce :ol.llx.agent/env
                                 (merge
                                  {:state_            (atom {:public-state state
                                                             :runtime      {:status      :running
                                                                            :closing?    false
                                                                            :next-run-id 1
                                                                            :active-run  nil}})
                                   :command>          command>
                                   :events-mx>        events-mx>
                                   :convert-to-llm    (or (:convert-to-llm opts) default-convert-to-llm)
                                   :transform-context (:transform-context opts)
                                   :stream-fn         (:stream-fn opts)
                                   :abort-signal      (:abort-signal opts)}
                                  optional-env)
                                 default-transformer
                                 {:registry schema-registry})]
      (driver/start! env)
      env)))

(>defn create-agent
       "Creates an agent runtime handle.

   Required options:
   - `:tools`             vector of tools

   `opts` may include:
   - `:convert-to-llm`    `(fn [messages])`; defaults to filtering
                          messages to roles `:user`, `:assistant`,
                          `:tool-result`
   - `:transform-context` optional context transform hook
   - `:stream-fn`         optional stream hook; resolved at callsite
   - `:schema-registry`   user-owned extra Malli schemas
   - `:custom-message-schemas` user-owned custom message dispatch map
   - `:session-id`, `:get-api-key`, `:thinking-budgets`, `:max-retry-delay-ms`
   - `:system-prompt`, `:model`, `:thinking-level`
   - `:steering-mode`, `:follow-up-mode`
   - `:abort-signal`

  For state rehydration, use [[rehydrate-agent]]."
       ([]
        [=> :ol.llx.agent/env]
        (create-agent {}))
       ([opts]
        [:ol.llx.agent/create-agent-opts => :ol.llx.agent/env]
        (let [schema-config   (schema-config opts)
              schema-registry (schema/derive-active-registry schema-config)
              opts'           (->> opts
                                   (schema/validate! schema-registry :ol.llx.agent/create-agent-opts))]
          (runtime-handle (create-initial-state schema-registry schema-config opts')
                          opts'))))

(defn rehydrate-agent
  "Creates an agent runtime from a previously persisted state snapshot.

   `state` must satisfy `:ol.llx.agent.loop/state`.
   `opts` carries runtime dependencies and may include:
   - `:convert-to-llm` (optional; see [[create-agent]])
   - `:transform-context` (optional; see [[create-agent]])
   - `:stream-fn` (optional; see [[create-agent]])
   - `:session-id`, `:get-api-key`, `:thinking-budgets`, `:max-retry-delay-ms`
  - `:tools`
  - `:abort-signal`."
  [state opts]
  (let [schema-registry (live-schema-registry state)]
    (runtime-handle state
                    (->> opts
                         (schema/validate! schema-registry :ol.llx.agent/create-agent-opts)))))

(defn state
  "Returns the current agent state snapshot."
  [agent]
  (:public-state @(:state_ agent)))

(defn- validate-with-agent-registry!
  [agent schema-id data]
  (schema/validate! (live-schema-registry (state agent)) schema-id data))

(defn subscribe
  "Subscribes a channel to the agent event stream.

   - `(subscribe agent)` creates and returns a buffered channel.
   - `(subscribe agent ch)` taps the provided channel and returns it."
  ([agent]
   (let [ch (sp/chan :buf (sp/sliding-buffer default-subscription-buffer-size))]
     (subscribe agent ch)))
  ([agent ch]
   (sp/tap (:events-mx> agent) ch true)
   ch))

(defn unsubscribe
  "Unsubscribes a channel from the agent event stream and closes it."
  [agent ch]
  (sp/untap (:events-mx> agent) ch)
  (sp/close ch)
  nil)

(defn- dispatch!
  [agent command]
  (let [schema-registry (live-schema-registry (state agent))]
    (schema/validate! schema-registry :ol.llx.agent/env agent)
    (schema/validate! schema-registry :ol.llx.agent/command command)
    (driver/run agent command)))

(defn- dispatch-queued-messages!
  [agent command-type messages]
  (let [messages (validate-with-agent-registry! agent :ol.llx.agent/messages (or messages []))]
    (p/loop [remaining messages]
      (if-let [message (first remaining)]
        (p/let [_ (dispatch! agent {:type command-type :message message})]
          (p/recur (rest remaining)))
        nil))))

(defn prompt
  "Submits a prompt command to the agent runtime."
  [agent messages]
  (dispatch! agent {:type     :ol.llx.agent.command/prompt
                    :messages (validate-with-agent-registry! agent :ol.llx.agent/messages (or messages []))}))

(defn continue
  "Submits a continue command to the agent runtime."
  [agent]
  (dispatch! agent {:type :ol.llx.agent.command/continue}))

(defn abort
  "Submits an abort command to the agent runtime."
  [agent]
  (dispatch! agent {:type :ol.llx.agent.command/abort}))

(defn steer
  "Queues steering messages."
  [agent messages]
  (dispatch-queued-messages! agent :ol.llx.agent.command/steer messages))

(defn follow-up
  "Queues follow-up messages."
  [agent messages]
  (dispatch-queued-messages! agent :ol.llx.agent.command/follow-up messages))

(defn wait-for-idle
  "Waits until the agent reaches `:ol.llx.agent.loop/idle` or `:ol.llx.agent.loop/closed`.

   Resolves `true` when idle/closed, or `false` after the polling budget is
   exhausted."
  ([agent]
   (wait-for-idle agent 6000))
  ([agent max-polls]
   (p/loop [remaining max-polls]
     (let [phase (::loop/phase (state agent))]
       (cond
         (or (= ::loop/idle phase) (= ::loop/closed phase))
         true

         (<= remaining 0)
         false

         :else
         (p/let [_ (p/delay 10)]
           (p/recur (dec remaining))))))))

(defn set-system-prompt
  [agent system-prompt]
  (dispatch! agent {:type          :ol.llx.agent.command/set-system-prompt
                    :system-prompt system-prompt}))

(defn set-model
  [agent model]
  (dispatch! agent {:type :ol.llx.agent.command/set-model :model model}))

(defn set-thinking-level
  [agent thinking-level]
  (dispatch! agent {:type           :ol.llx.agent.command/set-thinking-level
                    :thinking-level thinking-level}))

(defn set-tools
  [agent tools]
  (dispatch! agent {:type :ol.llx.agent.command/set-tools :tools tools}))

(defn- next-schema-config
  [current incoming]
  (reduce (fn [acc key]
            (let [value (get incoming key ::missing)]
              (if (or (= ::missing value) (nil? value))
                acc
                (assoc acc key value))))
          current
          [:schema-registry :custom-message-schemas]))

(defn set-schema-config
  "Replaces the agent's user-owned schema configuration.

  `schema-config-update` may include `:schema-registry` and/or
  `:custom-message-schemas`. `nil` for either field preserves the current
  user-owned value.

  This setter is accepted in any phase. It affects future validation and
  future tool execution only; already queued messages and already running
  tool executions keep using the registry they started with."
  [agent schema-config-update]
  (let [incoming          (m/coerce :ol.llx.agent/schema-config-update
                                    (or schema-config-update {})
                                    default-transformer
                                    {:registry base-schema-registry})
        current-config    (schema-config (state agent))
        next-config       (next-schema-config current-config incoming)
        schema-registry   (schema/derive-active-registry next-config)
        next-public-state (merge (state agent) next-config)]
    (schema/validate! schema-registry :ol.llx.agent/schema-config next-config)
    (schema/validate! schema-registry :ol.llx.agent.loop/state next-public-state)
    (dispatch! agent (assoc next-config :type :ol.llx.agent.command/set-schema-config))))

(defn set-steering-mode
  [agent mode]
  (dispatch! agent {:type :ol.llx.agent.command/set-steering-mode :mode mode}))

(defn set-follow-up-mode
  [agent mode]
  (dispatch! agent {:type :ol.llx.agent.command/set-follow-up-mode :mode mode}))

(defn replace-messages
  [agent messages]
  (dispatch! agent {:type     :ol.llx.agent.command/replace-messages
                    :messages (validate-with-agent-registry! agent :ol.llx.agent/messages messages)}))

(defn append-message
  [agent message]
  (dispatch! agent {:type    :ol.llx.agent.command/append-message
                    :message (validate-with-agent-registry! agent :ol.llx.agent/message message)}))

(defn clear-messages
  [agent]
  (dispatch! agent {:type :ol.llx.agent.command/clear-messages}))

(defn clear-steering-queue
  [agent]
  (dispatch! agent {:type :ol.llx.agent.command/clear-steering-queue}))

(defn clear-follow-up-queue
  [agent]
  (dispatch! agent {:type :ol.llx.agent.command/clear-follow-up-queue}))

(defn clear-all-queues
  [agent]
  (dispatch! agent {:type :ol.llx.agent.command/clear-all-queues}))

(defn reset
  [agent]
  (dispatch! agent {:type :ol.llx.agent.command/reset}))

(defn close
  "Closes the runtime."
  [agent]
  (dispatch! agent {:type :ol.llx.agent.command/close}))
