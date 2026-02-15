(ns llx.agent.fx
  "Effect interpreter and driver loop for the TEA agent.

   ## Architecture

   The agent uses The Elm Architecture (TEA) to separate pure state
   transitions (`llx.agent.loop`) from side effects (this namespace).

   The pure state machine in `loop.cljc` takes `(state, input)` and
   returns `[state', effects]`. Effects are inert data descriptions of
   work to perform. This namespace interprets those descriptions and
   feeds resulting signals back into the state machine.

   ## Effect categories

   Effects fall into two categories based on how they interact with
   the driver:

   Fire-and-forget — synchronous, produce no signals.
     - `:emit-event` — push an event to subscribers (UI, logging)
     - `:reject`     — log/warn about an invalid operation

   Signal-producing — async, return a channel of signals.
     - `:call-llm`     — stream an LLM response, emitting `llm-start`,
                          `llm-chunk`×N, then `llm-done` or `llm-error`
     - `:execute-tool`  — run a tool, emitting `tool-update`×N, then
                          `tool-result` or `tool-error`

   Signal-producing effects return a promesa CSP channel. The driver
   reads signals from it and steps each through `loop/step`. The effect
   interpreter never touches the state atom or calls `step` — the
   driver is the sole owner of that responsibility.

   ## Driver loop

   The driver (`run`) multiplexes across all active signal channels
   plus an external command channel (for abort, etc.) using `alts`.

   When a signal is stepped and produces new effects, fire-and-forget
   effects execute inline and signal-producing effects add their
   channel to the active set. When a channel closes, it is removed.
   The driver resolves when the active set is empty.

   Multiple signal-producing effects can be active simultaneously.
   This enables concurrent tool execution (e.g., multiple subagents
   running in parallel while the primary agent continues reasoning).
   The state machine processes signals one at a time regardless of
   how many channels are active — concurrency is in I/O, not in
   state transitions.

   ## Signal flow

   ```
   input (command/signal)
     → loop/step        → [state', effects]
     → driver           → interprets effects
       → fire-and-forget  → executed inline, no signals
       → signal-producing → channel added to active set
                              ↓
                           driver reads signal from channel
                              ↓
                           loop/step → [state', more effects]
                              ↓
                           ... continues until active set empty
   ```"
  (:require
   [clojure.string :as str]
   [llx.ai :as ai]
   [llx.agent.loop :as loop]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- fx-emit-event
  [env effect]
  (sp/offer (:events-mx> env) (:event effect))
  nil)

(defn- base-partial-assistant-message
  [model]
  (cond-> {:role :assistant :content []}
    (:api model) (assoc :api (:api model))
    (:provider model) (assoc :provider (:provider model))
    (:id model) (assoc :model (:id model))))

(defn- normalize-event-type
  [event]
  (let [t (:type event)]
    (cond
      (keyword? t) t
      (string? t) (-> t
                      (str/replace "_" "-")
                      keyword)
      :else t)))

(defn- ensure-text-block
  [partial]
  (let [content (:content partial)]
    (if (and (seq content) (= :text (:type (peek content))))
      partial
      (update partial :content conj {:type :text :text ""}))))

(defn- ensure-thinking-block
  [partial]
  (let [content (:content partial)]
    (if (and (seq content) (= :thinking (:type (peek content))))
      partial
      (update partial :content conj {:type :thinking :thinking ""}))))

(defn- update-last-text
  [partial text-delta]
  (let [partial (ensure-text-block partial)]
    (update-in partial [:content (dec (count (:content partial))) :text]
               (fnil str "")
               (or text-delta ""))))

(defn- update-last-thinking
  [partial thinking-delta]
  (let [partial (ensure-thinking-block partial)]
    (update-in partial [:content (dec (count (:content partial))) :thinking]
               (fnil str "")
               (or thinking-delta ""))))

(defn- upsert-tool-call
  [partial {:keys [id name arguments]}]
  (let [content (:content partial)
        idx     (first (keep-indexed (fn [i block]
                                       (when (and (= :tool-call (:type block))
                                                  (= id (:id block)))
                                         i))
                                     content))
        block   {:type      :tool-call
                 :id        id
                 :name      name
                 :arguments (or arguments {})}]
    (if (some? idx)
      (assoc-in partial [:content idx] block)
      (update partial :content conj block))))

(defn- llm-event->step
  [model partial event]
  (let [event-type (normalize-event-type event)
        partial    (or partial
                       (:partial event)
                       (base-partial-assistant-message model))]
    (case event-type
      :start
      {:partial partial
       :signal  {:type :signal/llm-start :message partial}
       :done?   false}

      :text-start
      (let [partial (or (:partial event)
                        (ensure-text-block partial))]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :text-delta
      (let [partial (or (:partial event)
                        (update-last-text partial (or (:text event) (:delta event))))]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :text-end
      (let [partial (or (:partial event) partial)]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :thinking-start
      (let [partial (or (:partial event)
                        (ensure-thinking-block partial))]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :thinking-delta
      (let [partial (or (:partial event)
                        (update-last-thinking partial (or (:thinking event) (:delta event))))]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :thinking-end
      (let [partial (or (:partial event) partial)]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :toolcall-start
      (let [partial (or (:partial event)
                        (upsert-tool-call partial {:id        (:id event)
                                                   :name      (:name event)
                                                   :arguments (:arguments event)}))]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :toolcall-delta
      (let [partial (or (:partial event)
                        (upsert-tool-call partial {:id        (:id event)
                                                   :name      (:name event)
                                                   :arguments (:arguments event)}))]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :toolcall-end
      (let [partial (or (:partial event)
                        (upsert-tool-call partial {:id        (:id event)
                                                   :name      (:name event)
                                                   :arguments (:arguments event)}))]
        {:partial partial
         :signal  {:type :signal/llm-chunk :chunk partial}
         :done?   false})

      :done
      {:partial partial
       :signal  {:type    :signal/llm-done
                 :message (or (:assistant-message event) (:message event) partial)}
       :done?   true}

      :error
      {:partial partial
       :signal  {:type  :signal/llm-error
                 :error (or (:assistant-message event) (:error event) event)}
       :done?   true}

      {:partial partial
       :signal  nil
       :done?   false})))

(defn- stream-options
  [{:keys [abort-signal] :as env}
   {:keys [thinking-level] :as _state}
   use-custom-stream-fn?
   resolved-api-key]
  (let [reasoning (when-not (= :off thinking-level)
                    thinking-level)
        base      (cond-> {}
                    reasoning (assoc :reasoning reasoning)
                    abort-signal (assoc :signal abort-signal))
        parity    (->> (select-keys env [:session-id
                                         :thinking-budgets
                                         :max-retry-delay-ms])
                       (remove (comp nil? val))
                       (into {}))]
    (cond-> (merge base parity)
      (some? resolved-api-key)
      (assoc :api-key resolved-api-key)

      (and use-custom-stream-fn?
           (some? (:get-api-key env)))
      (assoc :get-api-key (:get-api-key env)))))

(defn- default-stream-fn
  [model context options]
  (ai/stream (ai/default-env) model context options))

(defn- fx-call-llm
  [{:keys [state_ convert-to-llm transform-context stream-fn get-api-key abort-signal] :as env} effect]
  (let [out                                           (sp/chan)
        {:keys [model system-prompt tools] :as state} @state_
        use-custom-stream-fn?                         (fn? stream-fn)
        stream-fn                                     (or stream-fn default-stream-fn)
        context                                       (fn [llm-messages]
                                                        {:system-prompt system-prompt
                                                         :messages      llm-messages
                                                         :tools         tools})]
    (-> (p/let [messages         (if transform-context
                                   (transform-context (:messages effect) abort-signal)
                                   (:messages effect))
                llm-messages     (convert-to-llm messages)
                resolved-api-key (if get-api-key
                                   (get-api-key (name (:provider model)))
                                   nil)
                options          (stream-options env state use-custom-stream-fn? resolved-api-key)
                llm-context      (context llm-messages)
                stream-ch        (stream-fn model llm-context options)]
          (p/loop [partial nil]
            (p/let [event (sp/take stream-ch)]
              (if (nil? event)
                nil
                (let [{:keys [partial signal done?]} (llm-event->step model partial event)]
                  (if signal
                    (p/let [_ (sp/put out signal)]
                      (if done?
                        nil
                        (p/recur partial)))
                    (p/recur partial)))))))
        (p/catch (fn [error]
                   (sp/put out {:type :signal/llm-error :error error})))
        (p/finally (fn [_ _]
                     (sp/close out))))
    out))

(defn- fx-execute-tool
  [_env _effect]
  ;; TODO: look up tool from (:tools env), validate args, call execute
  ;; Returns a channel that emits signals:
  ;;   {:type :signal/tool-update ...}  (zero or more, for streaming tools)
  ;;   {:type :signal/tool-result ...}  or {:type :signal/tool-error ...}
  ;; Channel closes after the terminal result/error signal.
  ;; Check for steering messages after completion.
  (let [ch (sp/chan)]
    (sp/close ch)
    ch))

(defn- fx-reject
  [_env _effect]
  ;; TODO: log warning with (:reason effect)
  nil)

(defn execute-fx
  "Interpret a single effect description. Dispatches on `:fx/type`.

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
     - `:tools`              — map of tool-name → tool-def with `:execute` fn
     - `:abort-signal`       — abort token for cancellation"
  [env effect]
  (case (:fx/type effect)
    :emit-event (fx-emit-event env effect)
    :call-llm (fx-call-llm env effect)
    :execute-tool (fx-execute-tool env effect)
    :reject (fx-reject env effect)
    nil))

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
     (let [result (execute-fx env effect)]
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
   once, enabling concurrent tool execution (e.g., parallel subagents).

   Signal processing: When a signal arrives from any channel, it is
   stepped through `loop/step`. The resulting effects are processed:
   fire-and-forget effects execute inline, signal-producing effects add
   their channel to the active set. When a channel closes (effect
   complete), it is removed from the active set.

   Termination: The driver resolves when the active channel set is
   empty and all effects from the final step have been processed.

   `env`     — runtime dependencies (see `execute-fx`).
              Must include `:state_` (atom) and `:command>` (channel).
   `input`   — a command or signal map to process.

   Returns a promise that resolves when all effects and cascading
   signals have been fully processed."
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
              (do (run! sp/close active) nil) ;; command channel closed, tear down
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
