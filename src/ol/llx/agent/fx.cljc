(ns ol.llx.agent.fx
  "Effect interpreter for the agent runtime.

   The pure state machine in `ol.llx.agent.loop` emits inert effect maps.
   This namespace interprets those maps into concrete side effects.

   Effect categories:

   Fire-and-forget — synchronous, produce no signals.
     - `:emit-event` — publish an event to subscribers
     - `:reject`     — report invalid operation state

   Signal-producing — async, return an effect handle.
     - `:call-llm`     — perform inference and stream `llm-*` signals
     - `:execute-tool` — execute a tool and emit `tool-*` signals

   Signal-producing effects return maps containing a `:signals>` promesa CSP
   channel and a `:cancel!` function. `ol.llx.agent.driver/run` consumes the
   channel and retains the cancellation function."
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [ol.llx.agent.fx.inference :as inference]
   [ol.llx.agent.fx.tools :as tools]
   [ol.llx.agent.schema :as schema]
   [promesa.exec.csp :as sp]))

(defn- fx-emit-event
  [env effect]
  (sp/put (:events-mx> env) (:event effect))
  nil)

(defn- fx-reject
  [_env _effect]
  ;; TODO: log warning with (:reason effect)
  nil)

(>defn execute-fx
       "Interpret a single effect description. Dispatches on `::type`.

   Fire-and-forget effects (`:emit-event`, `:reject`):
   Execute the side effect synchronously. Return `nil`.

   Signal-producing effects (`:call-llm`, `:execute-tool`):
   Start the async work and return an effect handle containing a `:signals>`
   promesa CSP channel and a `:cancel!` function. The channel closes when the
   effect completes. The driver consumes the channel and retains the handle
   so it can cancel active work. The interpreter must not call `step` or
   mutate the state atom.

   `env` carries runtime dependencies:
     - `:state_`            — atom holding current agent state (read-only for fx)
     - `:events-mx>`        — event multiplexer write endpoint
     - `:convert-to-llm`    — `(fn [messages])` transform to LLM messages
     - `:transform-context`  — `(fn [messages abort-signal])` optional context pruning
     - `:stream-fn`          — `(fn [model context opts])` LLM streaming fn
     - `:abort-signal`       — abort token for cancellation"
       [env effect]
       [:ol.llx.agent/env ::effect => ::result]
       (let [schema-registry (schema/derive-active-registry (:public-state @(:state_ env)))]
         (schema/validate! schema-registry :ol.llx.agent/env env)
         (schema/validate! schema-registry ::effect effect)
         (case (::type effect)
           :emit-event (fx-emit-event env effect)
           :call-llm (inference/fx-call-llm env effect)
           :execute-tool (tools/fx-execute-tool env effect)
           :reject (fx-reject env effect)
           nil)))
