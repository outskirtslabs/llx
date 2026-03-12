(ns bramble-coding-agent.ui.editor
  (:require
   [clojure.string :as str]
   [com.phronemophobic.clobber.modes.text :as text])
  (:import
   [io.lacuna.bifurcan Rope]))

(defn- make-editor
  ([] (text/make-editor))
  ([initial-text]
   (if (seq (str initial-text))
     (text/editor-self-insert-command (text/make-editor) (str initial-text))
     (text/make-editor))))

(defn init
  ([id]
   (init id ""))
  ([id initial-text]
   {:id     id
    :editor (make-editor initial-text)}))

(defn text [state]
  (let [^Rope rope (get-in state [:editor :rope])]
    (if rope
      (.toString rope)
      "")))

(defn cursor-position [state]
  (let [cursor (get-in state [:editor :cursor])]
    {:char (:char cursor 0)
     :row  (:row cursor 0)
     :col  (:column-byte cursor 0)}))

(defn set-text [state new-text]
  (assoc state :editor (make-editor (str new-text))))

(defn clear [state]
  (assoc state :editor (text/editor-clear (:editor state))))

(defn set-cursor-row-col [state row col]
  (update state :editor text/editor-goto-row-col row col))

(defn- ctrl? [event]
  (contains? (:ol.tendril.event.key/modifiers event)
             :ol.tendril.event.key/control))

(defn- shift? [event]
  (contains? (:ol.tendril.event.key/modifiers event)
             :ol.tendril.event.key/shift))

(defn submit-event? [event]
  (and (= :ol.tendril.event.key/press (:ol.tendril.event.key/kind event))
       (= :ol.tendril.event.key/enter (:ol.tendril.event.key/code event))
       (not (ctrl? event))
       (not (shift? event))))

(defn- insert-text [state s]
  (update state :editor text/editor-self-insert-command (str s)))

(defn- handle-key-event [state event]
  (let [kind  (:ol.tendril.event.key/kind event)
        code  (:ol.tendril.event.key/code event)
        value (:ol.tendril.event.key/value event)]
    (if (not= kind :ol.tendril.event.key/press)
      state
      (cond
        (submit-event? event)
        state

        (= code :ol.tendril.event.key/backspace)
        (update state :editor text/editor-delete-backward-char)

        (= code :ol.tendril.event.key/delete)
        (update state :editor text/editor-delete-char)

        (= code :ol.tendril.event.key/left)
        (update state :editor text/editor-backward-char)

        (= code :ol.tendril.event.key/right)
        (update state :editor text/editor-forward-char)

        (= code :ol.tendril.event.key/up)
        (update state :editor text/editor-previous-line)

        (= code :ol.tendril.event.key/down)
        (update state :editor text/editor-next-line)

        (= code :ol.tendril.event.key/home)
        (update state :editor text/editor-move-beginning-of-line)

        (= code :ol.tendril.event.key/end)
        (update state :editor text/editor-move-end-of-line)

        (and (= code :ol.tendril.event.key/enter)
             (or (ctrl? event) (shift? event)))
        (insert-text state "\n")

        (and (char? value) (>= (int value) 32) (not (ctrl? event)))
        (insert-text state value)

        :else
        state))))

(defn handle-event [state event]
  (cond
    (= (:type event) :paste)
    (insert-text state (:content event))

    :else
    (handle-key-event state event)))

(defn visible-lines [state max-lines]
  (let [lines (str/split (text state) #"\n" -1)]
    (->> lines
         (take-last (max 1 max-lines))
         vec)))

(defn render
  [state & {:keys [focused? placeholder max-lines]
            :or   {focused?    true
                   placeholder ""
                   max-lines   5}}]
  (let [lines             (visible-lines state max-lines)
        txt               (if (seq (text state))
                            (str/join "\n" lines)
                            placeholder)
        total-lines       (count (str/split (text state) #"\n" -1))
        visible-count     (count lines)
        {:keys [row col]} (cursor-position state)
        visible-start     (max 0 (- total-lines visible-count))
        visible-row       (max 0 (- row visible-start))
        cursor-name       (:id state)
        display           [:text {:style {:fg [220 220 235]}}
                           (if (seq txt) txt placeholder)]]
    (if focused?
      [:with-cursor {:name  cursor-name
                     :row   visible-row
                     :col   col
                     :style :bar}
       display]
      display)))
