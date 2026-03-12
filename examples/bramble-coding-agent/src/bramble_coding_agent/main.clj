(ns bramble-coding-agent.main
  (:require
   [bramble-coding-agent.tools :as tools]
   [bramble-coding-agent.ui.editor :as editor]
   [bramble-coding-agent.ui.view :as view]
   [clojure.string :as str]
   [ol.bramble :as bramble]
   [ol.bramble.cursor :as cursor]
   [ol.llx.agent :as agent]
   [ol.llx.ai :as ai]
   [ol.tendril :as t]
   [ol.tendril.escape.csi :as csi]
   [ol.tendril.escape.dec :as dec]
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove]
   [town.lilac.flex :as flex])
  (:gen-class))

(def ^:private system-prompt
  "You are a helpful coding assistant. Use the read, write, edit, and bash tools when they help. Be concise and direct.")

(def spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(def ^:private exit-shortcut-threshold-ms
  1000)

(defn parse-args [args]
  (let [args (vec args)]
    (cond
      (empty? args)
      {:error "Usage: bramble-coding-agent --model <model-id>"}

      (not= "--model" (first args))
      {:error (str "Unknown argument: " (first args) "\nUsage: bramble-coding-agent --model <model-id>")}

      (< (count args) 2)
      {:error "Missing value for --model\nUsage: bramble-coding-agent --model <model-id>"}

      :else
      {:model-id (second args)})))

(defn collect-model-candidates []
  (->> (ai/get-providers)
       (mapcat ai/get-models)
       (sort-by (juxt (comp name :provider) :id))
       vec))

(defn resolve-model [model-id]
  (let [matches (->> (collect-model-candidates)
                     (filter #(= model-id (:id %)))
                     vec)]
    (case (count matches)
      0 {:error (str "Unknown model: \"" model-id "\"")}
      1 {:model (first matches)}
      {:error (str "Ambiguous model id \"" model-id "\"")})))

(defn extract-text [message]
  (->> (get message :content [])
       (filter #(= :text (:type %)))
       (map :text)
       (str/join "")))

(defn compute-accounting [messages]
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

(defn initial-ui-state [model]
  {:current-model            model
   :working?                 false
   :turns                    0
   :tokens                   {:input 0 :output 0 :total-tokens 0}
   :cost                     0.0
   :cwd                      (System/getProperty "user.dir")
   :transcript               []
   :spinner-frame            (first spinner-frames)
   :spinner-index            0
   :terminal-cols            80
   :terminal-rows            24
   :pending-exit-shortcut-at nil
   :streaming-text           ""
   :modal                    nil
   :model-picker-query       ""
   :model-picker-index       0
   :model-picker-scroll      0
   :model-picker-editor      (editor/init :model-picker-query "")
   :model-picker-candidates  []
   :editor                   (editor/init :composer "")})

(defn- set-state!
  [state_ value]
  (if (instance? clojure.lang.IAtom state_)
    (reset! state_ value)
    (state_ value)))

(defn- update-state!
  [state_ f & args]
  (if (instance? clojure.lang.IAtom state_)
    (apply swap! state_ f args)
    (state_ #(apply f % args))))

(defn- ctrl-key-press?
  [event ch]
  (and (= :ol.tendril.event.key/press (:ol.tendril.event.key/kind event))
       (= ch (:ol.tendril.event.key/value event))
       (contains? (:ol.tendril.event.key/modifiers event)
                  :ol.tendril.event.key/control)))

(defn apply-global-shortcut
  [state event now-ms]
  (cond
    (ctrl-key-press? event \c)
    (let [last-tap (:pending-exit-shortcut-at state)]
      (if (and last-tap
               (<= (- now-ms last-tap) exit-shortcut-threshold-ms))
        {:state  (assoc state :pending-exit-shortcut-at nil)
         :effect :exit}
        {:state  (assoc state :pending-exit-shortcut-at now-ms)
         :effect :clear-editor}))

    (ctrl-key-press? event \x)
    (let [last-tap (:pending-exit-shortcut-at state)]
      (if (and last-tap
               (<= (- now-ms last-tap) exit-shortcut-threshold-ms))
        {:state  (assoc state :pending-exit-shortcut-at nil)
         :effect :exit}
        {:state  (assoc state :pending-exit-shortcut-at now-ms)
         :effect nil}))

    :else
    {:state  state
     :effect nil}))

(defn- split-assistant-lines [text]
  (let [lines (str/split (or text "") #"\n" -1)]
    (cond
      (empty? lines) []
      (every? str/blank? lines) ["assistant>"]
      :else (into [(str "assistant> " (first lines))]
                  (rest lines)))))

(defn- split-user-lines [text]
  (let [lines (str/split (or text "") #"\n" -1)]
    (cond
      (empty? lines) []
      (every? str/blank? lines) ["user>"]
      :else (into [(str "user> " (first lines))]
                  (rest lines)))))

(defn apply-agent-event [state event]
  (case (:type event)
    :ol.llx.agent.event/agent-start
    (assoc state :working? true)

    :ol.llx.agent.event/message-start
    (if (= :assistant (get-in event [:message :role]))
      (assoc state :streaming-text "")
      state)

    :ol.llx.agent.event/message-update
    (assoc state :streaming-text (extract-text (:chunk event)))

    :ol.llx.agent.event/tool-execution-start
    (update state :transcript conj (str "tool> " (:tool-name event) " ..."))

    :ol.llx.agent.event/tool-execution-end
    (update state :transcript conj (str "tool> " (:tool-name event) " " (if (:is-error? event) "error" "ok")))

    :ol.llx.agent.event/message-end
    (case (get-in event [:message :role])
      :assistant
      (-> state
          (update :transcript into (split-assistant-lines (extract-text (:message event))))
          (assoc :streaming-text ""))

      :user
      (update state :transcript into (split-user-lines (:content (:message event))))

      state)

    :ol.llx.agent.event/agent-end
    (let [{:keys [turns tokens cost]} (compute-accounting (:messages event))]
      (assoc state
             :working? false
             :turns turns
             :tokens tokens
             :cost cost))

    state))

(defn- reset-state [state]
  (let [model (:current-model state)]
    (assoc (initial-ui-state model)
           :editor (editor/init :composer "")
           :cwd (:cwd state))))

(defn handle-submission!
  [{:keys [state_ agent exit! prompt! reset! set-model-picker! close-agent!]} submitted]
  (let [trimmed (str/trim submitted)]
    (cond
      (= trimmed "/clear")
      (do
        (reset! agent)
        (update-state! state_ reset-state))

      (= trimmed "/model")
      (set-model-picker!)

      (= trimmed "/quit")
      (do
        (close-agent! agent)
        (exit!))

      (seq trimmed)
      (prompt! agent [{:role      :user
                       :content   trimmed
                       :timestamp (System/currentTimeMillis)}])

      :else
      nil)))

(declare clamp-model-picker-scroll)

(defn- filtered-model-picker-candidates [state]
  (view/filtered-model-picker-candidates state))

(defn- open-model-picker! [state_]
  (let [candidates (collect-model-candidates)
        current    (:current-model @state_)
        selected   (or (first (keep-indexed (fn [idx model]
                                              (when (= model current) idx))
                                            candidates))
                       0)
        scroll     (clamp-model-picker-scroll
                    (assoc @state_
                           :model-picker-query ""
                           :model-picker-editor (editor/init :model-picker-query "")
                           :model-picker-candidates candidates)
                    (max 0 (inc (- selected (view/model-picker-page-size @state_)))))]
    (update-state! state_ assoc
                   :modal :model-picker
                   :model-picker-query ""
                   :model-picker-index selected
                   :model-picker-scroll scroll
                   :model-picker-editor (editor/init :model-picker-query "")
                   :model-picker-candidates candidates)))

(defn- close-model-picker! [state_]
  (update-state! state_ assoc :modal nil))

(defn- clamp-model-picker-index [state idx]
  (let [max-idx (max 0 (dec (count (filtered-model-picker-candidates state))))]
    (-> idx
        (max 0)
        (min max-idx))))

(defn- clamp-model-picker-scroll [state scroll]
  (let [page-size  (view/model-picker-page-size state)
        max-scroll (max 0 (- (count (filtered-model-picker-candidates state)) page-size))]
    (-> scroll
        (max 0)
        (min max-scroll))))

(defn- ensure-model-picker-selection-visible [state]
  (let [page-size (view/model-picker-page-size state)
        index     (:model-picker-index state)
        scroll    (:model-picker-scroll state)]
    (assoc state :model-picker-scroll
           (cond
             (< index scroll) index
             (>= index (+ scroll page-size)) (inc (- index page-size))
             :else (clamp-model-picker-scroll state scroll)))))

(defn- move-model-picker-selection [state delta]
  (-> state
      (assoc :model-picker-index
             (clamp-model-picker-index state (+ (:model-picker-index state) delta)))
      ensure-model-picker-selection-visible))

(defn- reset-model-picker-filter [state]
  (let [query (editor/text (:model-picker-editor state))]
    (-> state
        (assoc :model-picker-query query
               :model-picker-index 0
               :model-picker-scroll 0))))

(defn- update-model-picker-filter [state event]
  (-> state
      (update :model-picker-editor editor/handle-event event)
      reset-model-picker-filter))

(defn- page-model-picker-selection [state delta]
  (let [page-size  (view/model-picker-page-size state)
        new-scroll (clamp-model-picker-scroll state
                                              (+ (:model-picker-scroll state)
                                                 (* delta page-size)))]
    (-> state
        (assoc :model-picker-scroll new-scroll
               :model-picker-index (clamp-model-picker-index state
                                                             (+ (:model-picker-index state)
                                                                (* delta page-size))))
        ensure-model-picker-selection-visible)))

(defn- confirm-model-picker! [state_ the-agent]
  (let [state @state_
        model (nth (filtered-model-picker-candidates state)
                   (:model-picker-index state)
                   nil)]
    (when model
      (agent/set-model the-agent model)
      (update-state! state_
                     (fn [state]
                       (-> state
                           (assoc :current-model model
                                  :modal nil)
                           (update :transcript conj
                                   (str "system> model set to "
                                        (:id model)
                                        " ["
                                        (name (:provider model))
                                        "]"))))))))

(defn- handle-model-picker-event! [state_ the-agent event]
  (cond
    (= (:type event) :paste)
    (update-state! state_ update-model-picker-filter event)

    (= :ol.tendril.event.key/press (:ol.tendril.event.key/kind event))
    (case (:ol.tendril.event.key/code event)
      :ol.tendril.event.key/up
      (update-state! state_ move-model-picker-selection -1)

      :ol.tendril.event.key/down
      (update-state! state_ move-model-picker-selection 1)

      :ol.tendril.event.key/page-up
      (update-state! state_ page-model-picker-selection -1)

      :ol.tendril.event.key/page-down
      (update-state! state_ page-model-picker-selection 1)

      :ol.tendril.event.key/escape
      (close-model-picker! state_)

      :ol.tendril.event.key/enter
      (confirm-model-picker! state_ the-agent)

      (update-state! state_ update-model-picker-filter event))

    :else
    nil))

(defn- custom-terminal-setup! [terminal]
  (let [^java.io.OutputStream out (t/output-stream terminal)]
    (t/enter-raw terminal)
    (.write out (csi/utf8-bytes
                 (str
                  (dec/set-private-mode-str dec/show-cursor)
                  (csi/kitty-keyboard-push-flags-str
                   (csi/kitty-keyboard-flags
                    csi/kitty-keyboard-flag-disambiguate-escape-codes
                    csi/kitty-keyboard-flag-report-event-types))
                  (dec/set-private-mode-str dec/bracketed-paste)
                  (dec/set-private-mode-str dec/focus-tracking)
                  (dec/set-private-mode-str dec/in-band-resize-notifications))))
    (.flush out)
    out))

(defn- custom-terminal-teardown! [terminal ^java.io.OutputStream out]
  (.write out (csi/utf8-bytes
               (str
                (dec/reset-private-mode-str dec/in-band-resize-notifications)
                (dec/reset-private-mode-str dec/focus-tracking)
                (dec/reset-private-mode-str dec/bracketed-paste)
                (csi/kitty-keyboard-pop-flags-str)
                (dec/set-private-mode-str dec/show-cursor))))
  (.flush out)
  (t/exit-raw terminal))

(defn- run-event-consumer! [the-agent state_]
  (let [ch (agent/subscribe the-agent)]
    (loop []
      (when-let [event (sp/take! ch)]
        (update-state! state_ #(apply-agent-event % event))
        (recur)))))

(defn- handle-app-event [state_ the-agent exit! event]
  (let [{:keys [state effect]} (apply-global-shortcut @state_ event (System/currentTimeMillis))]
    (when (not= state @state_)
      (set-state! state_ state))
    (case effect
      :clear-editor
      (update-state! state_ update :editor editor/clear)

      :exit
      (exit!)

      nil
      (cond
        (= :tick (:type event))
        (update-state! state_
                       (fn [current-state]
                         (let [{:keys [cols rows]} (:dimensions event)
                               current-state       (cond-> current-state
                                                     cols (assoc :terminal-cols cols)
                                                     rows (assoc :terminal-rows rows))]
                           (if (:working? current-state)
                             (let [next-idx (mod (inc (:spinner-index current-state)) (count spinner-frames))]
                               (assoc current-state
                                      :spinner-index next-idx
                                      :spinner-frame (nth spinner-frames next-idx)))
                             current-state))))

        (= :model-picker (:modal @state_))
        (handle-model-picker-event! state_ the-agent event)

        (editor/submit-event? event)
        (let [submitted (editor/text (:editor @state_))]
          (handle-submission!
           {:state_            state_
            :agent             the-agent
            :exit!             exit!
            :prompt!           agent/prompt
            :reset!            agent/reset
            :set-model-picker! #(open-model-picker! state_)
            :close-agent!      agent/close}
           submitted)
          (update-state! state_ update :editor editor/clear))

        (or (= (:type event) :paste)
            (:ol.tendril.event.key/code event))
        (update-state! state_ update :editor editor/handle-event event)

        :else
        nil))))

(defn- make-app [state_ the-agent exit?_]
  {:render            #(view/root-view @state_)
   :cursor            (fn [cursor-requests]
                        ((cursor/cursor-by-name
                          (if (= :model-picker (:modal @state_))
                            :model-picker-query
                            :composer))
                         cursor-requests))
   :terminal-setup    custom-terminal-setup!
   :terminal-teardown custom-terminal-teardown!
   :handle-event      (fn [_ctx event]
                        (handle-app-event state_ the-agent #(reset! exit?_ true) event)
                        (when @exit?_ [:ol.bramble/halt]))})

(defn -main [& args]
  (let [{:keys [model-id error]} (parse-args args)]
    (when error
      (println error)
      (System/exit 1))
    (let [{:keys [model error]} (resolve-model model-id)]
      (when error
        (println error)
        (System/exit 1))
      (trove/set-log-fn! nil)
      (let [state_    (flex/source (initial-ui-state model))
            exit?_    (atom false)
            the-agent (agent/create-agent {:model         model
                                           :system-prompt system-prompt
                                           :tools         (tools/make-tools)})]
        (doto (Thread. #(run-event-consumer! the-agent state_))
          (.setDaemon true)
          (.start))
        (try
          (bramble/run-app (make-app state_ the-agent exit?_))
          (finally
            (agent/close the-agent)))))))
