(ns llx.agent.fx
  "Effect execution layer for `llx.agent.fsm`.

  The FSM plans effect descriptors as data (`{:op ...}` maps). This namespace
  executes those descriptors by dispatching to handlers."
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.agent.fsm :as fsm]
   [promesa.core :as p]))

(def op->handler-key
  {fsm/fx-start-turn          :start-turn!
   fsm/fx-finish-turn         :finish-turn!
   fsm/fx-invoke-runner       :invoke-runner!
   fsm/fx-cancel-runner       :cancel-runner!
   fsm/fx-resolve-command     :resolve-command!
   fsm/fx-reject-command      :reject-command!
   fsm/fx-resolve-active      :resolve-active!
   fsm/fx-reject-active       :reject-active!
   fsm/fx-resolve-waiters     :resolve-waiters!
   fsm/fx-reject-waiters      :reject-waiters!
   fsm/fx-enqueue-idle-waiter :enqueue-idle-waiter!
   fsm/fx-enqueue-steering    :enqueue-steering!
   fsm/fx-enqueue-follow-up   :enqueue-follow-up!
   fsm/fx-close-runtime       :close-runtime!})

(def known-ops
  (set (keys op->handler-key)))

(def command->event
  {:prompt                      fsm/cmd-prompt
   :continue                    fsm/cmd-continue
   :steer                       fsm/cmd-steer
   :follow-up                   fsm/cmd-follow-up
   :abort                       fsm/cmd-abort
   :wait                        fsm/cmd-wait
   :reset                       fsm/cmd-reset
   :shutdown                    fsm/cmd-shutdown
   :llx.agent.command/prompt    fsm/cmd-prompt
   :llx.agent.command/continue  fsm/cmd-continue
   :llx.agent.command/steer     fsm/cmd-steer
   :llx.agent.command/follow-up fsm/cmd-follow-up
   :llx.agent.command/abort     fsm/cmd-abort
   :llx.agent.command/wait      fsm/cmd-wait
   :llx.agent.command/reset     fsm/cmd-reset
   :llx.agent.command/shutdown  fsm/cmd-shutdown})

(>defn handlers-from-callbacks
       "Builds an op-keyed handler map from callback-keyed functions.

  Callback keys are the values of `op->handler-key`, e.g. `:invoke-runner!`."
       [callbacks]
       [:llx.agent.fx/callbacks => :llx.agent.fx/handlers]
       (reduce-kv
        (fn [handlers op callback-key]
          (if-let [handler (get callbacks callback-key)]
            (assoc handlers op handler)
            handlers))
        {}
        op->handler-key))

(>defn handler-key
       [op]
       [:llx.agent.fsm/effect-op => [:maybe :llx.agent.fx/callback-key]]
       (get op->handler-key op))

(defn- effect-op
  [effect]
  (:op effect))

(>defn resolve-handler
       "Resolves a handler for an effect op from execution context.

  Resolution order:
  1. `[:handlers op]` (op-keyed map)
  2. callback key on context map (e.g. `:invoke-runner!`)"
       [ctx op]
       [:llx.agent.fx/context :llx.agent.fsm/effect-op => [:maybe [:fn fn?]]]
       (or (get-in ctx [:handlers op])
           (when-let [callback-key (handler-key op)]
             (get ctx callback-key))))

(defn- missing-handler-error
  [ctx effect]
  (let [op (effect-op effect)]
    (ex-info
     "No handler registered for effect op."
     {:type               :llx.agent.fx/missing-handler
      :op                 op
      :effect             effect
      :handler-key        (handler-key op)
      :known-op?          (contains? known-ops op)
      :available-handlers (set (keys (:handlers ctx)))})))

(defn- merged-context
  [ctx]
  (let [from-callbacks (handlers-from-callbacks ctx)]
    (assoc ctx :handlers (merge from-callbacks (:handlers ctx)))))

(>defn command-event
       "Returns the FSM event keyword associated with a runtime command keyword."
       [command]
       [:keyword => [:maybe :llx.agent.fsm/event-id]]
       (get command->event command))

(>defn execute-effect!
       "Executes a single effect descriptor.

  Handler signature:
  - `(fn [ctx effect] ...)`

  Returns a deferred that resolves to:
  `{:effect effect :status :ok :value any}`

  Missing handlers default to error; set `:on-missing-handler` on `ctx` to
  `:ignore` to skip unknown effects."
       [ctx effect]
       [:llx.agent.fx/context :llx.agent.fsm/effect => :llx/deferred]
       (let [op      (effect-op effect)
             handler (resolve-handler ctx op)]
         (cond
           (not (map? effect))
           (p/rejected
            (ex-info
             "Effect must be a map."
             {:type   :llx.agent.fx/invalid-effect
              :effect effect}))

           (nil? op)
           (p/rejected
            (ex-info
             "Effect is missing :op."
             {:type   :llx.agent.fx/missing-op
              :effect effect}))

           (fn? handler)
           (-> (p/resolved nil)
               (p/then (fn [_]
                         (handler ctx effect)))
               (p/then (fn [value]
                         {:effect effect
                          :status :ok
                          :value  value})))

           (= :ignore (:on-missing-handler ctx))
           (p/resolved
            {:effect effect
             :status :ignored
             :reason :missing-handler})

           :else
           (p/rejected (missing-handler-error ctx effect)))))

(>defn execute-effects!
       "Executes effects in order.

  Returns a deferred of a vector of execution result maps."
       ([ctx effects]
        [:llx.agent.fx/context :llx.agent.fsm/effects => :llx/deferred]
        (execute-effects! ctx effects {}))
       ([ctx effects {:keys [continue-on-error?] :or {continue-on-error? false}}]
        [:llx.agent.fx/context :llx.agent.fsm/effects :llx.agent.fx/effects-options => :llx/deferred]
        (reduce
         (fn [results* effect]
           (p/then results*
                   (fn [results]
                     (-> (execute-effect! ctx effect)
                         (p/then (fn [result]
                                   (conj results result)))
                         (p/catch (fn [ex]
                                    (if continue-on-error?
                                      (p/resolved
                                       (conj results
                                             {:effect effect
                                              :status :error
                                              :error  ex}))
                                      (p/rejected ex))))))))
         (p/resolved [])
         effects)))

(>defn execute-step!
       "Executes a full FSM step map (from `llx.agent.fsm/send!`)."
       ([ctx step]
        [:llx.agent.fx/context :llx.agent.fsm/step => :llx/deferred]
        (execute-step! ctx step {}))
       ([ctx step opts]
        [:llx.agent.fx/context :llx.agent.fsm/step :llx.agent.fx/effects-options => :llx/deferred]
        (-> (execute-effects! ctx (:effects step) opts)
            (p/then (fn [effect-results]
                      {:step           step
                       :effect-results effect-results})))))

(>defn dispatch-event!
       "Dispatches an FSM event and executes the resulting planned effects.

  This is the bridge from state transition to side-effect execution in one call.
  It mutates the FSM env through `llx.agent.fsm/send!`, then executes the
  returned `:effects` vector through this namespace's effect handlers."
       ([ctx env event-or-name]
        [:llx.agent.fx/context :llx.agent.fsm/env :llx.agent.fsm/event-or-name => :llx/deferred]
        (dispatch-event! ctx env event-or-name nil))
       ([ctx env event-or-name payload]
        [:llx.agent.fx/context :llx.agent.fsm/env :llx.agent.fsm/event-or-name [:maybe :map] => :llx/deferred]
        (let [step     (if (some? payload)
                         (fsm/send! env event-or-name payload)
                         (fsm/send! env event-or-name))
              exec-ctx (merged-context ctx)]
          (execute-step! exec-ctx step))))

(>defn dispatch-command!
       "Dispatches a high-level command keyword through the FSM+FX pipeline."
       ([ctx env command]
        [:llx.agent.fx/context :llx.agent.fsm/env :keyword => :llx/deferred]
        (dispatch-command! ctx env command nil))
       ([ctx env command payload]
        [:llx.agent.fx/context :llx.agent.fsm/env :keyword [:maybe :map] => :llx/deferred]
        (if-let [event-id (command-event command)]
          (dispatch-event! ctx env event-id payload)
          (p/rejected
           (ex-info "Unknown command keyword."
                    {:type    :llx.agent.fx/unknown-command
                     :command command})))))
