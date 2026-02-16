(ns llx.agent.demo
  (:require
   [clojure.java.io :as io]
   [llx.agent :as agent]
   [llx.ai :as ai]
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

(defn- start-event-forwarder!
  [runtime]
  (let [events> (agent/subscribe runtime)
        worker  (future
                  (loop []
                    (when-some [event (p/await (sp/take events>))]
                      (tap> event)
                      (recur))))]
    {:events> events>
     :worker  worker}))

(defn- stop-event-forwarder!
  [runtime {:keys [events> worker]}]
  (agent/unsubscribe runtime events>)
  (deref worker 1000 nil)
  nil)

(defn- read-file-tool
  []
  {:name         "read_file"
   :description  "Read a UTF-8 file from disk and return its contents."
   :input-schema [:map
                  [:path
                   {:description "Absolute or relative file path to read."}
                   :string]]})

(defn- read-file-tool-def
  []
  (assoc (read-file-tool)
         :execute (fn [_tool-call-id {:keys [path]} _abort-signal on-update]
                    (on-update {:stage :reading :path path})
                    {:content [{:type :text
                                :text (slurp path)}]})))

(comment
  ;; Evaluate in REPL:
  ;; (require '[llx.agent.demo :as demo] :reload)

  ;; Optional event tap setup.
  (def tap-handler (make-file-tap-handler tap-log-file))
  (io/make-parents tap-log-file)
  (spit tap-log-file "")
  (add-tap tap-handler)

  ;; Local file used by the tool.
  (spit "tmp/llx-agent-demo-input.txt"
        "The build succeeded.\nThe next step is to ship it.\n")

  ;; Register tool for runtime execution and model-visible tool schema list.
  (def read-file-def (read-file-tool-def))
  (def opts {:tool-defs      {"read_file" read-file-def}
             :system-prompt  "Use tools when needed and keep answers concise."
             :model          (ai/get-model :openai "gpt-5.2-codex")
             :thinking-level :medium
             :tools          [read-file-def]})

  (def runtime (agent/create-agent opts))
  (def forwarder (start-event-forwarder! runtime))

  ;; Driver-managed path.
  (p/await
   (agent/prompt runtime
                 [{:role      :user
                   :content   "Use the read_file tool to read deps.edn, then summarize it in one sentence."
                   :timestamp (System/currentTimeMillis)}]))

  ;; Manual stepper path (optional): same behavior, explicit loop stepping.
  ;; (p/await
  ;;  (run-command! runtime
  ;;                {:type     :llx.agent.command/prompt
  ;;                 :messages [{:role      :user
  ;;                             :content   "Use read_file on tmp/llx-agent-demo-input.txt and return the first line only."
  ;;                             :timestamp (System/currentTimeMillis)}]}))

  (-> runtime agent/state :messages)

  ;; Cleanup.
  (do
    (stop-event-forwarder! runtime forwarder)
    (p/await (agent/close runtime)))

  ;;
  )
