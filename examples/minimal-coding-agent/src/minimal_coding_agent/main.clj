(ns minimal-coding-agent.main
  (:require
   [clojure.string :as str]
   [minimal-coding-agent.tools :as tools]
   [ol.llx.agent :as agent]
   [ol.llx.ai :as ai]
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove])
  (:import
   [org.jline.reader EndOfFileException LineReaderBuilder UserInterruptException]
   [org.jline.terminal TerminalBuilder]
   [org.jline.utils AttributedStringBuilder AttributedStyle Status])
  (:gen-class))

(def ^:private system-prompt
  "You are a helpful coding assistant. You have access to tools that let you read files, write files, edit files, and run shell commands. Use these tools when they help you answer the user's request. Be concise and direct.")

(defn parse-args
  "Parse CLI args. Returns {:model-id str} or {:error str}."
  [args]
  (let [args (vec args)]
    (cond
      (empty? args)
      {:error "Usage: minimal-coding-agent --model <model-id>"}

      (not= "--model" (first args))
      {:error (str "Unknown argument: " (first args) "\nUsage: minimal-coding-agent --model <model-id>")}

      (< (count args) 2)
      {:error "Missing value for --model\nUsage: minimal-coding-agent --model <model-id>"}

      :else
      {:model-id (second args)})))

(defn resolve-model
  "Find a unique model by id across all providers.
   Returns {:model map} or {:error str}."
  [model-id]
  (let [matches (->> (ai/get-providers)
                     (mapcat ai/get-models)
                     (filter #(= model-id (:id %)))
                     vec)]
    (case (count matches)
      0 {:error (str "Unknown model: \"" model-id "\"\n"
                     "Run with a valid model id. Examples:\n"
                     "  claude-3-5-sonnet-20241022\n"
                     "  gpt-4o\n"
                     "  gemini-1.5-pro")}
      1 {:model (first matches)}
      {:error (str "Ambiguous model id \"" model-id "\" — found in "
                   (count matches) " providers: "
                   (str/join ", " (map (comp name :provider) matches))
                   "\nSpecify a more unique model id.")})))

(defn extract-text
  "Extract concatenated text content from an assistant message or chunk."
  [message]
  (->> (get message :content [])
       (filter #(= :text (:type %)))
       (map :text)
       (str/join "")))

(defn compute-accounting
  "Derive turn count, token totals, and cost from the current message list."
  [messages]
  (let [assistant-msgs (filter #(= :assistant (:role %)) messages)]
    {:turns  (count assistant-msgs)
     :tokens (reduce (fn [acc msg]
                       (let [u (or (:usage msg) {})]
                         (-> acc
                             (update :input + (or (:input u) 0))
                             (update :output + (or (:output u) 0))
                             (update :total-tokens + (or (:total-tokens u) 0)))))
                     {:input 0 :output 0 :total-tokens 0}
                     assistant-msgs)
     :cost   (reduce (fn [acc msg]
                       (+ acc (or (get-in msg [:usage :cost :total]) 0.0)))
                     0.0
                     assistant-msgs)}))

(def ^:private initial-ui-state
  {:working? false
   :turns    0
   :tokens   {:input 0 :output 0 :total-tokens 0}
   :cost     0.0})

(def ^:private initial-stream-buf
  {:text "" :printed-len 0})

(defn- format-tokens [n]
  (if (>= n 1000)
    (format "%.1fk" (/ (double n) 1000.0))
    (str n)))

(defn- format-cost [n]
  (format "$%.4f" n))

(defn- shorten-path [path]
  (let [home (System/getProperty "user.home")]
    (if (str/starts-with? path home)
      (str "~" (subs path (count home)))
      path)))

(def ^:private spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defn update-status!
  "Refresh the JLine status footer (1 line: model | cwd | turns | tokens | cost)."
  [status ui-state_ model]
  (when status
    (let [{:keys [turns tokens cost]} @ui-state_
          model-id                    (:id model)
          cwd                         (shorten-path (System/getProperty "user.dir"))
          tok-str                     (format-tokens (:total-tokens tokens))
          cost-str                    (format-cost cost)
          sb                          (AttributedStringBuilder.)]
      (.style sb (.foreground AttributedStyle/DEFAULT AttributedStyle/CYAN))
      (.append sb model-id)
      (.style sb AttributedStyle/DEFAULT)
      (.append sb (str " │ " cwd " │ " turns " turns │ " tok-str " tokens │ " cost-str))
      (.update status [(.toAttributedString sb)]))))

(defn- spinner-prompt
  "Build the readline prompt string. When working, prepend an animated spinner line."
  [frame]
  (str frame " working…\nyou> "))

(defn run-spinner!
  "Animate a spinner above the readline prompt while the agent is working.
   Updates the LineReader prompt via setPrompt + callWidget(redisplay).
   This is the sole writer of both the prompt and the status bar."
  [status ui-state_ model line-reader]
  (loop [i 0 was-working? false]
    (let [working? (:working? @ui-state_)]
      (cond
        working?
        (do (.setPrompt line-reader (spinner-prompt (nth spinner-frames (mod i (count spinner-frames)))))
            (try (.callWidget line-reader "redisplay")
                 (catch IllegalStateException _ nil))
            (Thread/sleep 80)
            (recur (inc i) true))
        was-working?
        (do (.setPrompt line-reader "you> ")
            (try (.callWidget line-reader "redisplay")
                 (catch IllegalStateException _ nil))
            (update-status! status ui-state_ model)
            (recur 0 false))
        :else
        (do (Thread/sleep 200)
            (recur 0 false))))))

(defn handle-event!
  "Map one agent event to terminal output. Called on the event consumer thread."
  [event line-reader _status ui-state_ stream-buf_ _model]
  (case (:type event)

    :ol.llx.agent.event/agent-start
    (swap! ui-state_ assoc :working? true)

    :ol.llx.agent.event/message-start
    (when (= :assistant (get-in event [:message :role]))
      (reset! stream-buf_ initial-stream-buf))

    :ol.llx.agent.event/message-update
    (let [full-text (extract-text (:chunk event))
          prev-len  (:printed-len @stream-buf_)
          delta     (subs full-text (min prev-len (count full-text)))]
      (when (seq delta)
        ;; print only complete lines as they arrive; hold partial line in buffer
        (let [last-nl (str/last-index-of delta "\n")]
          (if (some? last-nl)
            (let [to-print (subs delta 0 (inc last-nl))]
              (.printAbove line-reader to-print)
              (swap! stream-buf_ assoc
                     :text full-text
                     :printed-len (+ prev-len (inc last-nl))))
            (swap! stream-buf_ assoc :text full-text)))))

    :ol.llx.agent.event/message-end
    (let [msg (:message event)]
      (when (= :assistant (:role msg))
        ;; flush any buffered partial line
        (let [{:keys [text printed-len]} @stream-buf_
              remaining                  (subs text (min printed-len (count text)))]
          (when (seq remaining)
            (.printAbove line-reader remaining)))
        (.printAbove line-reader "")
        (reset! stream-buf_ initial-stream-buf)))

    :ol.llx.agent.event/tool-execution-start
    (.printAbove line-reader (str "⟳ " (:tool-name event) " …"))

    :ol.llx.agent.event/tool-execution-end
    (.printAbove line-reader (str (if (:is-error? event) "✗" "✓")
                                  " " (:tool-name event)))

    :ol.llx.agent.event/agent-end
    (let [acct (compute-accounting (:messages event))]
      (swap! ui-state_ assoc
             :working? false
             :turns    (:turns acct)
             :tokens   (:tokens acct)
             :cost     (:cost acct)))

    nil))

(defn run-event-consumer!
  "Consume agent events on the current thread until the channel closes."
  [the-agent line-reader status ui-state_ stream-buf_ model]
  (let [ch (agent/subscribe the-agent)]
    (loop []
      (let [event (sp/take! ch)]
        (when event
          (handle-event! event line-reader status ui-state_ stream-buf_ model)
          (recur))))))

(defn -main [& args]
  (let [{:keys [model-id error]} (parse-args args)]
    (when error
      (println error)
      (System/exit 1))
    (let [{:keys [model error]} (resolve-model model-id)]
      (when error
        (println error)
        (System/exit 1))
      ;; Silence Trove observability signals — they would print to stdout and
      ;; clobber the JLine terminal layout.
      (trove/set-log-fn! nil)
      (let [the-agent   (agent/create-agent {:model         model
                                             :system-prompt system-prompt
                                             :tools         (tools/make-tools)})
            terminal    (-> (TerminalBuilder/builder) (.build))
            line-reader (-> (LineReaderBuilder/builder)
                            (.terminal terminal)
                            (.build))
            status      (Status/getStatus terminal)
            ui-state_   (atom initial-ui-state)
            stream-buf_ (atom initial-stream-buf)]

        ;; start the event consumer on a background daemon thread
        (doto (Thread. #(run-event-consumer! the-agent line-reader status ui-state_ stream-buf_ model))
          (.setDaemon true)
          (.start))

        ;; spinner thread — owns the readline prompt and the status bar
        (doto (Thread. #(run-spinner! status ui-state_ model line-reader))
          (.setDaemon true)
          (.start))

        ;; Status construction calls change_scroll_region (DECSTBM), which on
        ;; virtually all terminals resets the cursor to (0,0). Move the cursor
        ;; to the bottom of the scroll region so readLine renders the prompt
        ;; there instead of at the top. With 1 status line the scroll region
        ;; ends at row (height - 1), 1-indexed.
        (let [rows (.getHeight terminal)
              tw   (.writer terminal)]
          (when (> rows 2)
            (.print tw (str "\033[" (- rows 1) ";1H"))
            (.flush tw)))

        (.printAbove line-reader (str "Minimal coding agent  │  model: " (:id model)
                                      "\nType /quit to exit, /clear to reset the session."))

        (loop []
          (let [line (try
                       (.readLine line-reader "you> ")
                       (catch UserInterruptException _
                         nil)
                       (catch EndOfFileException _
                         :eof))]
            (cond
              (= line :eof)
              (do @(agent/close the-agent)
                  (System/exit 0))

              (nil? line)
              (recur)

              (= "/quit" (str/trim line))
              (do @(agent/close the-agent)
                  (System/exit 0))

              (= "/clear" (str/trim line))
              (do @(agent/reset the-agent)
                  (reset! ui-state_ initial-ui-state)
                  (reset! stream-buf_ initial-stream-buf)
                  (.printAbove line-reader "─── session cleared ───")
                  (recur))

              (str/blank? line)
              (recur)

              :else
              (let [msg [{:role :user :content line :timestamp (System/currentTimeMillis)}]]
                (if (:working? @ui-state_)
                  (do (agent/steer the-agent msg)
                      (.printAbove line-reader "↳ queued")
                      (recur))
                  (do (agent/prompt the-agent msg)
                      (recur)))))))))))
