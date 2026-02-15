(ns llx.agent.driver
  "TEA driver loop for the agent runtime.

   The driver is the sole owner of state transitions. It steps inputs
   through `llx.agent.loop/step`, interprets resulting effects via
   `llx.agent.fx/execute-fx`, and feeds resulting signals back into
   `loop/step`.

   Multiplexing:
   - Tracks all active signal channels returned by signal-producing effects.
   - Includes `command>` in the `alts` set for external commands.
   - Removes channels when they close.

   Signal flow:

   input (command/signal)
     -> loop/step        -> [state', effects]
     -> execute-fx       -> channels + fire-and-forget side effects
     -> alts over active channels + command>
     -> loop/step for each received signal
     -> repeat until active channel set is empty."
  (:require
   [llx.agent.fx :as fx]
   [llx.agent.loop :as loop]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- step-and-collect!
  "Run `loop/step` against the current state atom, update the atom,
   and return the effects vector."
  [state_ input]
  (let [[state' effects] (loop/step @state_ input)]
    (reset! state_ state')
    effects))

(defn- process-effects!
  "Interpret a vector of effects. Executes fire-and-forget effects
   inline and collects channels from signal-producing effects.
   Returns a vector of signal channels (may be empty)."
  [env effects]
  (reduce
   (fn [channels effect]
     (let [result (fx/execute-fx env effect)]
       (if (sp/chan? result)
         (conj channels result)
         channels)))
   []
   effects))

(defn run
  "TEA driver loop. Processes a single input through the state machine,
   interprets resulting effects, and multiplexes signals from all active
   channels back through `loop/step` until the system settles.

   The driver is the sole owner of state transitions — it is the only
   code that calls `loop/step` and mutates the state atom. Effect
   interpreters perform side effects but never step the state machine.

   Multiplexing: The driver maintains a set of active signal
   channels (from `:call-llm`, `:execute-tool`, etc.) plus the external
   `command>` channel. It uses `alts` to read from all of them
   concurrently. Multiple signal-producing effects can be active at
   once, enabling concurrent tool execution.

   Signal processing: When a signal arrives from any channel, it is
   stepped through `loop/step`. The resulting effects are processed:
   fire-and-forget effects execute inline, signal-producing effects add
   their channel to the active set. When a channel closes (effect
   complete), it is removed from the active set.

   Termination: The driver resolves when the active channel set is
   empty and all effects from the final step have been processed."
  [env input]
  (let [state_   (:state_ env)
        command> (:command> env)
        effects  (step-and-collect! state_ input)
        new-chs  (process-effects! env effects)]
    (p/loop [active (set new-chs)]
      (when (seq active)
        (p/let [ports      (cond-> (vec active)
                             command> (conj command>))
                [val port] (sp/alts ports)]
          (cond
            ;; External command (abort, etc.)
            (and command> (= port command>))
            (if (nil? val)
              (do (run! sp/close active) nil)
              (let [effects (step-and-collect! state_ val)
                    new-chs (process-effects! env effects)]
                (p/recur (into active new-chs))))

            ;; Signal channel closed — effect is complete, remove it
            (nil? val)
            (p/recur (disj active port))

            ;; Signal from an active effect — step it
            :else
            (let [effects (step-and-collect! state_ val)
                  new-chs (process-effects! env effects)]
              (p/recur (into active new-chs)))))))))
