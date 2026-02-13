(ns llx.agent.fsm
  "Exploratory runtime state machine model for agent command/turn lifecycle.

  This namespace separates:

  1. Control flow state transitions (modeled with Fulcro Statecharts).
  2. Side-effect planning (modeled as explicit data descriptors).

  It does not execute side effects yet; it only emits effect plans."
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [com.fulcrologic.statecharts :as sc]
   [com.fulcrologic.statecharts.chart :as chart]
   [com.fulcrologic.statecharts.elements :refer [state transition]]
   [com.fulcrologic.statecharts.events :as events]
   [com.fulcrologic.statecharts.protocols :as sp]
   [com.fulcrologic.statecharts.runtime :as sc-runtime]
   [com.fulcrologic.statecharts.simple :as simple]
   [llx.agent.schema :as agent-schema]))

;; State IDs
(def runtime ::runtime)
(def idle ::idle)
(def starting ::starting)
(def running ::running)
(def closed ::closed)

;; Chart runtime IDs
(def chart-src ::runtime-chart)
(def default-session-id ::runtime-session)

;; Command event IDs
(def cmd-prompt ::cmd-prompt)
(def cmd-continue ::cmd-continue)
(def cmd-steer ::cmd-steer)
(def cmd-follow-up ::cmd-follow-up)
(def cmd-abort ::cmd-abort)
(def cmd-wait ::cmd-wait)
(def cmd-reset ::cmd-reset)
(def cmd-shutdown ::cmd-shutdown)

;; Lifecycle event IDs
(def runner-started ::runner-started)
(def runner-start-failed ::runner-start-failed)
(def runner-succeeded ::runner-succeeded)
(def runner-failed ::runner-failed)

;; Effect op IDs
(def fx-start-turn ::fx-start-turn)
(def fx-finish-turn ::fx-finish-turn)
(def fx-invoke-runner ::fx-invoke-runner)
(def fx-cancel-runner ::fx-cancel-runner)
(def fx-resolve-command ::fx-resolve-command)
(def fx-reject-command ::fx-reject-command)
(def fx-resolve-active ::fx-resolve-active)
(def fx-reject-active ::fx-reject-active)
(def fx-resolve-waiters ::fx-resolve-waiters)
(def fx-reject-waiters ::fx-reject-waiters)
(def fx-enqueue-idle-waiter ::fx-enqueue-idle-waiter)
(def fx-enqueue-steering ::fx-enqueue-steering)
(def fx-enqueue-follow-up ::fx-enqueue-follow-up)
(def fx-close-runtime ::fx-close-runtime)

(def all-command-events
  #{cmd-prompt cmd-continue cmd-steer cmd-follow-up cmd-abort cmd-wait cmd-reset cmd-shutdown})

(def all-events
  (into all-command-events
        [runner-started runner-start-failed runner-succeeded runner-failed]))

(>defn effect
       "Builds a normalized effect descriptor map.

  Effects are modeled as plain data so planning stays pure and easy to inspect.
  The `:op` key identifies what should happen later, while all other keys carry
  parameters for the effect interpreter."
       ([op]
        [:llx.agent.fsm/effect-op => :llx.agent.fsm/effect]
        {:op op})
       ([op attrs]
        [:llx.agent.fsm/effect-op :map => :llx.agent.fsm/effect]
        (assoc attrs :op op)))

(>defn event
       "Builds a normalized event map for chart dispatch.

  The FSM accepts both bare event keywords and explicit event maps. This helper
  makes event payload handling uniform so tests, demos, and future runtime code
  can use the same shape without ad hoc branching."
       ([event-name]
        [:llx.agent.fsm/event-id => :llx.agent.fsm/event]
        {:name event-name :data {}})
       ([event-name data]
        [:llx.agent.fsm/event-id :map => :llx.agent.fsm/event]
        {:name event-name :data (or data {})}))

(>defn event-name
       "Extracts an event keyword from either a keyword or event map.

  This keeps transition planning tolerant to both calling styles, which is
  useful when mixing command envelopes and direct keyword dispatch."
       [event-or-name]
       [:llx.agent.fsm/event-or-name => :llx.agent.fsm/event-id]
       (if (keyword? event-or-name)
         event-or-name
         (:name event-or-name)))

(defn- internal
  [event-name]
  (transition {:event event-name :type :internal}))

(def runtime-statechart
  (chart/statechart {}
                    (state {:id      runtime
                            :initial idle}
                           (state {:id idle}
                                  (transition {:event cmd-prompt :target starting})
                                  (transition {:event cmd-continue :target starting})
                                  (internal cmd-steer)
                                  (internal cmd-follow-up)
                                  (internal cmd-abort)
                                  (internal cmd-wait)
                                  (internal cmd-reset)
                                  (transition {:event cmd-shutdown :target closed}))
                           (state {:id starting}
                                  (internal cmd-prompt)
                                  (internal cmd-continue)
                                  (internal cmd-steer)
                                  (internal cmd-follow-up)
                                  (internal cmd-abort)
                                  (internal cmd-wait)
                                  (transition {:event runner-started :target running})
                                  (transition {:event runner-start-failed :target idle})
                                  (transition {:event cmd-reset :target idle})
                                  (transition {:event cmd-shutdown :target closed}))
                           (state {:id running}
                                  (internal cmd-prompt)
                                  (internal cmd-continue)
                                  (internal cmd-steer)
                                  (internal cmd-follow-up)
                                  (internal cmd-abort)
                                  (internal cmd-wait)
                                  (transition {:event runner-succeeded :target idle})
                                  (transition {:event runner-failed :target idle})
                                  (transition {:event cmd-reset :target idle})
                                  (transition {:event cmd-shutdown :target closed}))
                           (state {:id closed}
                                  (internal cmd-prompt)
                                  (internal cmd-continue)
                                  (internal cmd-steer)
                                  (internal cmd-follow-up)
                                  (internal cmd-abort)
                                  (internal cmd-wait)
                                  (internal cmd-reset)
                                  (internal cmd-shutdown)))))

(def transition-targets
  {idle
   {cmd-prompt    starting
    cmd-continue  starting
    cmd-steer     idle
    cmd-follow-up idle
    cmd-abort     idle
    cmd-wait      idle
    cmd-reset     idle
    cmd-shutdown  closed}

   starting
   {cmd-prompt          starting
    cmd-continue        starting
    cmd-steer           starting
    cmd-follow-up       starting
    cmd-abort           starting
    cmd-wait            starting
    runner-started      running
    runner-start-failed idle
    cmd-reset           idle
    cmd-shutdown        closed}

   running
   {cmd-prompt       running
    cmd-continue     running
    cmd-steer        running
    cmd-follow-up    running
    cmd-abort        running
    cmd-wait         running
    runner-succeeded idle
    runner-failed    idle
    cmd-reset        idle
    cmd-shutdown     closed}

   closed
   {cmd-prompt    closed
    cmd-continue  closed
    cmd-steer     closed
    cmd-follow-up closed
    cmd-abort     closed
    cmd-wait      closed
    cmd-reset     closed
    cmd-shutdown  closed}})

(def transition-effects
  {idle
   {cmd-prompt
    (fn [payload]
      [(effect fx-start-turn)
       (effect fx-invoke-runner {:command :prompt :payload payload})])

    cmd-continue
    (fn [payload]
      [(effect fx-start-turn)
       (effect fx-invoke-runner {:command :continue :payload payload})])

    cmd-steer
    (fn [payload]
      [(effect fx-enqueue-steering {:messages payload})
       (effect fx-resolve-command {:value true})])

    cmd-follow-up
    (fn [payload]
      [(effect fx-enqueue-follow-up {:messages payload})
       (effect fx-resolve-command {:value true})])

    cmd-abort
    (fn [_]
      [(effect fx-resolve-command {:value false})])

    cmd-wait
    (fn [_]
      [(effect fx-resolve-command {:value true})])

    cmd-reset
    (fn [_]
      [(effect fx-resolve-waiters)
       (effect fx-resolve-command {:value :reset-snapshot})])

    cmd-shutdown
    (fn [_]
      [(effect fx-reject-waiters {:reason :runtime-closed})
       (effect fx-resolve-command {:value true})
       (effect fx-close-runtime)])}

   starting
   {cmd-prompt
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-busy})])

    cmd-continue
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-busy})])

    cmd-steer
    (fn [payload]
      [(effect fx-enqueue-steering {:messages payload})
       (effect fx-resolve-command {:value true})])

    cmd-follow-up
    (fn [payload]
      [(effect fx-enqueue-follow-up {:messages payload})
       (effect fx-resolve-command {:value true})])

    cmd-abort
    (fn [_]
      [(effect fx-cancel-runner)
       (effect fx-resolve-command {:value true})])

    cmd-wait
    (fn [_]
      [(effect fx-enqueue-idle-waiter)])

    runner-started
    (fn [_]
      [])

    runner-start-failed
    (fn [payload]
      [(effect fx-finish-turn {:error payload})
       (effect fx-resolve-waiters)
       (effect fx-reject-active {:error payload})])

    cmd-reset
    (fn [_]
      [(effect fx-cancel-runner)
       (effect fx-reject-active {:reason :runtime-reset})
       (effect fx-resolve-waiters)
       (effect fx-resolve-command {:value :reset-snapshot})])

    cmd-shutdown
    (fn [_]
      [(effect fx-cancel-runner)
       (effect fx-reject-active {:reason :runtime-closed})
       (effect fx-reject-waiters {:reason :runtime-closed})
       (effect fx-resolve-command {:value true})
       (effect fx-close-runtime)])}

   running
   {cmd-prompt
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-busy})])

    cmd-continue
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-busy})])

    cmd-steer
    (fn [payload]
      [(effect fx-enqueue-steering {:messages payload})
       (effect fx-resolve-command {:value true})])

    cmd-follow-up
    (fn [payload]
      [(effect fx-enqueue-follow-up {:messages payload})
       (effect fx-resolve-command {:value true})])

    cmd-abort
    (fn [_]
      [(effect fx-cancel-runner)
       (effect fx-resolve-command {:value true})])

    cmd-wait
    (fn [_]
      [(effect fx-enqueue-idle-waiter)])

    runner-succeeded
    (fn [payload]
      [(effect fx-finish-turn)
       (effect fx-resolve-waiters)
       (effect fx-resolve-active {:value payload})])

    runner-failed
    (fn [payload]
      [(effect fx-finish-turn {:error payload})
       (effect fx-resolve-waiters)
       (effect fx-reject-active {:error payload})])

    cmd-reset
    (fn [_]
      [(effect fx-cancel-runner)
       (effect fx-reject-active {:reason :runtime-reset})
       (effect fx-resolve-waiters)
       (effect fx-resolve-command {:value :reset-snapshot})])

    cmd-shutdown
    (fn [_]
      [(effect fx-cancel-runner)
       (effect fx-reject-active {:reason :runtime-closed})
       (effect fx-reject-waiters {:reason :runtime-closed})
       (effect fx-resolve-command {:value true})
       (effect fx-close-runtime)])}

   closed
   {cmd-prompt
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-closed})])

    cmd-continue
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-closed})])

    cmd-steer
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-closed})])

    cmd-follow-up
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-closed})])

    cmd-abort
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-closed})])

    cmd-wait
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-closed})])

    cmd-reset
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-closed})])

    cmd-shutdown
    (fn [_]
      [(effect fx-reject-command {:reason :runtime-closed})])}})

(>defn new-env
       "Creates a statechart environment handle for this runtime model.

  The returned map holds the statechart system environment and a `session-id`
  for one machine instance. This is the mutable runtime bucket for chart state
  transitions"
       ([]
        [=> :llx.agent.fsm/env]
        (new-env {}))
       ([opts]
        [:llx.agent.fsm/new-env-options => :llx.agent.fsm/env]
        (let [{:keys [session-id system-env]
               :or   {session-id default-session-id}}
              (agent-schema/validate! :llx.agent.fsm/new-env-options opts)
              system-env                                                   (or system-env (simple/simple-env))]
          (simple/register! system-env chart-src runtime-statechart)
          {:system-env system-env
           :session-id session-id
           :chart-src  chart-src})))

(>defn start!
       "Starts the FSM environment and returns it.

  The explicit start step mirrors real runtime lifecycle expectations and makes
  setup order visible in tests and REPL walkthroughs."
       [env]
       [:llx.agent.fsm/env => :llx.agent.fsm/env]
       (simple/start! (:system-env env) (:chart-src env) (:session-id env))
       env)

(>defn active-states
       "Returns the raw active state configuration from the chart runtime.

  Use this when you need full visibility into the chart configuration instead of
  the simplified single-phase helper [[phase]]."
       [env]
       [:llx.agent.fsm/env => [:set :llx.agent.fsm/state-id]]
       (or (sc-runtime/current-configuration (:system-env env) (:session-id env))
           #{}))

(>defn in-state?
       "Checks whether `env` currently includes `state-id` in its active set.

  This is a convenience query for assertions and diagnostics when validating
  transition behavior."
       [env state-id]
       [:llx.agent.fsm/env :llx.agent.fsm/state-id => :boolean]
       (contains? (active-states env) state-id))

(>defn phase
       "Returns the current high-level runtime phase keyword.

  The chart tracks an active set, but most runtime logic only cares about one
  top-level lifecycle phase. This projects the active set onto `:closed`,
  `:running`, `:starting`, or `:idle` for simpler reasoning."
       [env]
       [:llx.agent.fsm/env => [:maybe :llx.agent.fsm/state-id]]
       (let [active (active-states env)]
         (some #(when (contains? active %) %)
               [closed running starting idle])))

(>defn supported-events
       "Returns the set of event keywords recognized for `state-id`.

  This is derived from `transition-targets` and is useful for UI introspection,
  guardrails, and debugging unsupported command dispatches."
       [state-id]
       [:llx.agent.fsm/state-id => [:set :llx.agent.fsm/event-id]]
       (set (keys (get transition-targets state-id))))

(>defn transition-plan
       "Computes planned transition and effect data for a state/event pair.

  This function is the pure planning core. It does not mutate chart state or
  execute side effects. It answers: given current state and event payload, what
  state should we move to and which effects should be requested?"
       ([state-id event-or-name]
        [:llx.agent.fsm/state-id :llx.agent.fsm/event-or-name => :llx.agent.fsm/transition-plan]
        (transition-plan state-id event-or-name nil))
       ([state-id event-or-name payload]
        [:llx.agent.fsm/state-id :llx.agent.fsm/event-or-name [:maybe :map] => :llx.agent.fsm/transition-plan]
        (let [nm           (event-name event-or-name)
              next-state   (get-in transition-targets [state-id nm] state-id)
              effects-fn   (get-in transition-effects [state-id nm])
              effect-items (if (fn? effects-fn) (vec (effects-fn payload)) [])]
          {:state              state-id
           :event              nm
           :payload            payload
           :planned-next-state next-state
           :effects            effect-items
           :supported?         (contains? (supported-events state-id) nm)})))

(>defn effects-for
       "Returns only the planned effect descriptors for a transition lookup.

  This is a narrow convenience wrapper around [[transition-plan]] for callers
  that only need effect planning output."
       ([state-id event-or-name]
        [:llx.agent.fsm/state-id :llx.agent.fsm/event-or-name => :llx.agent.fsm/effects]
        (:effects (transition-plan state-id event-or-name nil)))
       ([state-id event-or-name payload]
        [:llx.agent.fsm/state-id :llx.agent.fsm/event-or-name [:maybe :map] => :llx.agent.fsm/effects]
        (:effects (transition-plan state-id event-or-name payload))))

(>defn send!
       "Dispatches an event to the chart and returns a transition report map.

  `send!` bridges pure planning with live chart progression. It first computes a
  plan (`:planned-next-state`, `:effects`, `:supported?`), then mutates the
  chart environment, and finally reports both before/after phases. It still does
  not execute effects; execution is delegated to `llx.agent.fx`."
       ([env event-or-name]
        [:llx.agent.fsm/env :llx.agent.fsm/event-or-name => :llx.agent.fsm/step]
        (send! env event-or-name nil))
       ([env event-or-name payload]
        [:llx.agent.fsm/env :llx.agent.fsm/event-or-name [:maybe :map] => :llx.agent.fsm/step]
        (let [before                                       (phase env)
              nm                                           (event-name event-or-name)
              payload                                      (if (and (nil? payload) (map? event-or-name))
                                                             (:data event-or-name)
                                                             payload)
              plan                                         (transition-plan before nm payload)
              statechart-evt                               (if (some? payload)
                                                             (event nm payload)
                                                             nm)
              system-env                                   (:system-env env)
              session-id                                   (:session-id env)
              {::sc/keys [processor working-memory-store]} system-env
              wmem                                         (sp/get-working-memory working-memory-store system-env session-id)]
          (when-not wmem
            (throw (ex-info "FSM env has not been started. Call start! before send!."
                            {:type       :llx.agent.fsm/not-started
                             :session-id session-id})))
          (let [wmem2 (sp/process-event! processor system-env wmem (events/new-event statechart-evt))]
            (sp/save-working-memory! working-memory-store system-env session-id wmem2))
          (assoc plan
                 :before before
                 :after (phase env)
                 :active-states (active-states env)))))
