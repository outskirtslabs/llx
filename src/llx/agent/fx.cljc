(ns llx.agent.fx
  "Effect interpreter for the TEA agent.

   The pure state machine in `llx.agent.loop` emits inert effect maps.
   This namespace interprets those maps into concrete side effects.

   Effect categories:

   Fire-and-forget — synchronous, produce no signals.
     - `:emit-event` — publish an event to subscribers
     - `:reject`     — report invalid operation state

   Signal-producing — async, return a channel of signals.
     - `:call-llm`     — perform inference and stream `llm-*` signals
     - `:execute-tool` — execute a tool and emit `tool-*` signals

   Signal-producing effects return promesa CSP channels consumed by
   `llx.agent.driver/run`."
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.agent.fx.inference :as inference]
   [llx.agent.fx.tools :as tools]
   [llx.agent.schema :as schema]
   [promesa.exec.csp :as sp]))

(defn- fx-emit-event
  [env effect]
  (sp/offer (:events-mx> env) (:event effect))
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
   Start the async work and return a promesa CSP channel that will
   emit signal maps. The channel closes when the effect is complete.
   The driver consumes the channel and steps each signal through
   `loop/step`. The interpreter must not call `step` or mutate the
   state atom.

   `env` carries runtime dependencies:
     - `:state_`            — atom holding current agent state (read-only for fx)
     - `:events-mx>`        — event multiplexer write endpoint
     - `:convert-to-llm`    — `(fn [messages])` transform to LLM messages
     - `:transform-context`  — `(fn [messages abort-signal])` optional context pruning
     - `:stream-fn`          — `(fn [model context opts])` LLM streaming fn
     - `:tools`              — vector of tools
     - `:abort-signal`       — abort token for cancellation"
       [env effect]
       [:llx.agent/env ::effect => ::result]
       (let [schema-registry (:schema-registry env)]
         (schema/validate! schema-registry :llx.agent/env env)
         (schema/validate! schema-registry ::effect effect)
         (case (::type effect)
           :emit-event (fx-emit-event env effect)
           :call-llm (inference/fx-call-llm env effect)
           :execute-tool (tools/fx-execute-tool env effect)
           :reject (fx-reject env effect)
           nil)))
