(ns llx.agent.demo
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [llx.agent :as agent]
   [llx.agent.fx.inference :as inference]
   [llx.agent.loop :as loop]
   [llx.ai :as ai]
   [llx.agent.schema :as schema]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(set! *warn-on-reflection* true)

(def tap-log-file
  "tmp/llx-agent-demo.tap.edn")

(defn- append-line!
  [path value]
  (io/make-parents path)
  (spit path (str (pr-str value) "\n") :append true))

(defn make-file-tap-handler
  "Returns a tap handler that appends each tapped value as one EDN line."
  [path]
  (fn [value]
    (append-line! path value)))

(defn- latest-assistant-message
  [state]
  (->> (:messages state)
       (filter (fn [message]
                 (= :assistant (:role message))))
       last))

(defn- demo-convert-to-llm
  [messages]
  (->> messages
       (filter (fn [message]
                 (#{:user :assistant :tool-result} (:role message))))
       vec))

(defn- render-demo-response
  [context]
  (let [latest-user-content (->> (:messages context)
                                 (filter (fn [message]
                                           (= :user (:role message))))
                                 last
                                 :content
                                 str
                                 str/lower-case)]
    (cond
      (str/includes? latest-user-content "two-word codename")
      "Signal Harbor"

      (str/includes? latest-user-content "explain")
      "It suggests a clear channel where useful ideas arrive and anchor quickly."

      :else
      "Ready.")))

(defn- demo-stream-fn
  [_model context _options]
  (let [out  (sp/chan)
        text (render-demo-response context)]
    (future
      (p/await (sp/put out {:type :start}))
      (p/await (sp/put out {:type :text-start}))
      (p/await (sp/put out {:type :text-delta :text text}))
      (p/await (sp/put out {:type :text-end}))
      (p/await (sp/put out {:type :done}))
      (sp/close out))
    out))

(defn- start-event-forwarder!
  [runtime]
  (let [events> (agent/subscribe runtime)
        worker  (future
                  (loop []
                    (when-some [event (p/await (sp/take events>))]
                      (tap> {:kind  :llx.agent/event
                             :event event})
                      (recur))))]
    {:events> events>
     :worker  worker}))

(defn- stop-event-forwarder!
  [runtime {:keys [events> worker]}]
  (agent/unsubscribe runtime events>)
  (deref worker 1000 nil)
  nil)

(defn- step-and-collect!
  [state_ input]
  (let [[state' effects] (loop/step @state_ input)]
    (reset! state_ state')
    effects))

(defn- execute-fx-for-demo
  [env effect]
  (case (:llx.agent.fx/type effect)
    :emit-event
    (do
      (sp/offer (:events-mx> env) (:event effect))
      nil)

    :call-llm
    (inference/fx-call-llm env effect)

    :execute-tool
    (let [ch (sp/chan)]
      (sp/close ch)
      ch)

    :reject
    nil

    nil))

(defn- process-effects!
  [env effects]
  (reduce
   (fn [channels effect]
     (let [result (execute-fx-for-demo env effect)]
       (if (sp/chan? result)
         (conj channels result)
         channels)))
   []
   effects))

(defn- run-command!
  [env input]
  (let [state_   (:state_ env)
        effects  (step-and-collect! state_ input)
        new-chs  (process-effects! env effects)]
    (p/loop [active (set new-chs)]
      (when (seq active)
        (p/let [[val port] (sp/alts (vec active))]
          (if (nil? val)
            (p/recur (disj active port))
            (let [effects (step-and-collect! state_ val)
                  new-chs (process-effects! env effects)]
              (p/recur (into active new-chs)))))))))

(defn run-demo!
  "Runs a deterministic two-turn agent session with no tools.

   Emits all runtime events via `tap>` and appends tapped values to
   `tap-log-file`."
  []
  (let [model       (ai/get-model :openai "gpt-5.2-codex")
        opts        {:tool-defs       {}
                     :system-prompt  "You are concise and never call tools."
                     :model          model
                     :convert-to-llm demo-convert-to-llm
                     :stream-fn      demo-stream-fn
                     :thinking-level :off
                     :tools          []}
        schema-rg   (schema/registry {:custom-message-schemas (:custom-message-schemas opts)})
        init-state  (agent/create-initial-state schema-rg opts)
        runtime     (agent/rehydrate-agent init-state opts)
        tap-handler (make-file-tap-handler tap-log-file)
        now         (System/currentTimeMillis)
        turn-1      {:role      :user
                     :content   "Give me a two-word codename for this conversation."
                     :timestamp now}
        turn-2      {:role      :user
                     :content   "Now explain that codename in one short sentence."
                     :timestamp (inc now)}
        forwarder   (start-event-forwarder! runtime)]
    (io/make-parents tap-log-file)
    (spit tap-log-file "")
    (add-tap tap-handler)
    (tap> {:kind :llx.agent/demo :status :start})
    (try
      (p/await (run-command! runtime {:type :llx.agent.command/prompt :messages [turn-1]}))
      (when-not (= ::loop/idle (::loop/phase (agent/state runtime)))
        (throw (ex-info "Turn 1 did not settle to idle" {:phase (::loop/phase (agent/state runtime))})))
      (let [state-1             (agent/state runtime)
            assistant-message-1 (latest-assistant-message state-1)]
        (tap> {:kind :llx.agent/demo :status :turn-1-done :assistant assistant-message-1})
        (p/await (run-command! runtime {:type :llx.agent.command/prompt :messages [turn-2]}))
        (when-not (= ::loop/idle (::loop/phase (agent/state runtime)))
          (throw (ex-info "Turn 2 did not settle to idle" {:phase (::loop/phase (agent/state runtime))})))
        (let [state-2             (agent/state runtime)
              assistant-message-2 (latest-assistant-message state-2)
              result              {:assistant-turn-1 assistant-message-1
                                   :assistant-turn-2 assistant-message-2
                                   :message-count    (count (:messages state-2))
                                   :tap-log-file     tap-log-file}]
          (tap> {:kind :llx.agent/demo :status :done :result result})
          result))
      (finally
        (remove-tap tap-handler)
        (stop-event-forwarder! runtime forwarder)
        (p/await (agent/close runtime))))))

(comment
  ;; Evaluate in REPL:
  ;; (require '[llx.agent.demo :as demo] :reload)
  ;; (demo/run-demo!)
  ;;
  ;; Then inspect:
  ;; (slurp demo/tap-log-file)
  )
