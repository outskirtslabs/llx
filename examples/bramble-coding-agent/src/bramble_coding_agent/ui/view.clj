(ns bramble-coding-agent.ui.view
  (:require
   [bramble-coding-agent.ui.editor :as editor]
   [bramble-coding-agent.ui.overlay :as overlay]
   [clojure.string :as str]))

(defn- format-tokens [n]
  (if (>= n 1000)
    (format "%.1fk" (/ (double n) 1000.0))
    (str n)))

(defn- format-cost [n]
  (format "$%.4f" (double n)))

(defn status-line [{:keys [current-model working? turns tokens cost cwd]}]
  (str (:id current-model)
       " | "
       (name (:provider current-model))
       " | "
       (if working? "working" "idle")
       " | "
       turns
       " turns | "
       (format-tokens (:total-tokens tokens))
       " tokens | "
       (format-cost cost)))

(defn shorten-path [path]
  (let [home (System/getProperty "user.home")]
    (if (and path (str/starts-with? path home))
      (str "~" (subs path (count home)))
      path)))

(defn status-lines [{:keys [cwd] :as state}]
  [(str "cwd: " (shorten-path cwd))
   (status-line state)])

(defn spinner-line [{:keys [working? spinner-frame]}]
  (if working?
    (str spinner-frame " Working")
    "Idle"))

(defn- split-long-token [token width]
  (if (<= (count token) width)
    [token]
    (->> token
         (partition-all width)
         (mapv #(apply str %)))))

(defn- wrap-line [line width]
  (let [width (max 1 width)]
    (if (<= (count line) width)
      [line]
      (loop [remaining (remove empty? (str/split (or line "") #" "))
             current   ""
             out       []]
        (if-let [token (first remaining)]
          (let [pieces    (split-long-token token width)
                token     (first pieces)
                rest'     (concat (rest pieces) (rest remaining))
                candidate (if (seq current)
                            (str current " " token)
                            token)]
            (if (<= (count candidate) width)
              (recur rest' candidate out)
              (recur remaining "" (conj out current))))
          (cond-> out
            (seq current) (conj current)))))))

(defn wrap-transcript-lines [lines width]
  (->> lines
       (mapcat #(wrap-line % width))
       vec))

(defn- transcript-content-width [{:keys [terminal-cols] :or {terminal-cols 80}}]
  (max 1 (- terminal-cols 2)))

(defn- transcript-view [{:keys [transcript] :as state}]
  (let [wrapped-lines (wrap-transcript-lines transcript (transcript-content-width state))]
    [:box {:id :transcript-box :width Integer/MAX_VALUE}
     [:vstack {:cross-axis-alignment :start}
      (if (seq transcript)
        (for [line wrapped-lines]
          ^{:key line}
          [:text line])
        [:text {:style {:fg [120 120 145]}} "No transcript yet."])]]))

(defn composer-view [{:keys [editor]}]
  [:box {:id :composer-box :border :single :width Integer/MAX_VALUE :height 5}
   [:padding {:left 1 :right 1 :top 0 :bottom 0}
    (if editor
      (editor/render editor :focused? true :placeholder "Type a message..." :max-lines 5)
      [:text {:style {:fg [120 120 145]}} "Type a message..."])]])

(defn model-picker-query [{:keys [model-picker-query model-picker-editor]}]
  (let [editor-query (some-> model-picker-editor editor/text)]
    (if (seq editor-query)
      editor-query
      (or model-picker-query ""))))

(defn model-picker-editor-state [{:keys [model-picker-editor] :as state}]
  (let [query (model-picker-query state)]
    (if (and model-picker-editor
             (= query (editor/text model-picker-editor)))
      model-picker-editor
      (editor/init :model-picker-query query))))

(defn filtered-model-picker-candidates
  [{:keys [model-picker-candidates] :as state}]
  (let [query (str/lower-case (model-picker-query state))
        terms (remove str/blank? (str/split query #"\s+"))]
    (->> model-picker-candidates
         (filter (fn [{:keys [id provider] model-name :name}]
                   (let [haystack (str/lower-case
                                   (str id " " (or model-name "") " " (clojure.core/name provider)))]
                     (every? #(str/includes? haystack %) terms))))
         vec)))

(defn model-picker-page-size [{:keys [terminal-rows] :or {terminal-rows 24}}]
  (max 1 (- terminal-rows 8)))

(defn visible-model-picker-candidates
  [{:keys [model-picker-scroll] :as state}]
  (let [filtered (filtered-model-picker-candidates state)
        start    (max 0 (or model-picker-scroll 0))
        end      (min (count filtered)
                      (+ start (model-picker-page-size state)))]
    (subvec filtered start end)))

(defn selected-model-picker-candidate
  [{:keys [model-picker-index] :as state}]
  (nth (filtered-model-picker-candidates state) model-picker-index nil))

(defn model-picker-summary [{:keys [model-picker-index] :as state}]
  (let [filtered (filtered-model-picker-candidates state)]
    (str "("
         (if (seq filtered) (inc model-picker-index) 0)
         "/"
         (count filtered)
         ")")))

(defn model-picker-dialog
  [{:keys [current-model model-picker-index model-picker-scroll model-picker-editor] :as state}]
  (let [visible-candidates (visible-model-picker-candidates state)
        selected-candidate (selected-model-picker-candidate state)]
    [:box {:border :double :width 64}
     [:padding {:left 1 :right 1 :top 0 :bottom 0}
      [:vstack {:cross-axis-alignment :start}
       [:hstack {:cross-axis-alignment :center}
        [:text {:style {:fg [247 230 70] :bold true}} "> "]
        (editor/render (model-picker-editor-state state)
                       :focused? true
                       :placeholder ""
                       :max-lines 1)]
       [:text {:style {:fg [120 120 145]}}
        "Up/Down navigate, PgUp/PgDn jump, Enter selects, Esc aborts"]
       (for [[visible-idx candidate] (map-indexed vector visible-candidates)
             :let                    [idx (+ (or model-picker-scroll 0) visible-idx)]]
         ^{:key (str (:provider candidate) "/" (:id candidate))}
         [:text {:style (cond-> {:fg [220 220 235]}
                          (= idx model-picker-index) (assoc :fg [100 210 255] :bold true)
                          (= current-model candidate) (assoc :fg [160 230 140]))}
          (str (if (= idx model-picker-index) "→ " "  ")
               (:id candidate)
               " ["
               (name (:provider candidate))
               "]"
               (when (= current-model candidate) " ✓"))])
       (when-not (seq visible-candidates)
         [:text {:style {:fg [120 120 145]}} "No matching models"])
       [:text {:style {:fg [120 120 145]}}
        (model-picker-summary state)]
       [:text {:style {:fg [120 120 145]}}
        (str "Model Name: " (or (:name selected-candidate) "-"))]]]]))

(defn- modal-view [state]
  (when (= :model-picker (:modal state))
    [:center
     (model-picker-dialog state)]))

(defn app-view [state]
  (let [[cwd-line meta-line] (status-lines state)]
    [:box {:style {:bg [18 20 24]}}
     [:padding {:left 1 :right 1 :top 0 :bottom 0}
      [:vstack {:gap 1}
       [:flexible
        (transcript-view state)]
       [:text {:style {:fg [120 120 145]}}
        (spinner-line state)]
       (composer-view state)
       [:text {:style {:fg [120 120 145]}}
        cwd-line]
       [:text {:style {:fg [120 120 145]}}
        meta-line]]]]))

(defn root-view [state]
  (overlay/present (app-view state) (modal-view state)))
