(ns bramble-coding-agent.ui.view-test
  (:require
   [bramble-coding-agent.ui.view :as sut]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [ol.bramble :as bramble]
   [ol.bramble.frame.compose :as compose]))

(def base-state
  {:current-model           {:id "gpt-test" :provider :openai}
   :working?                false
   :turns                   3
   :tokens                  {:total-tokens 1200}
   :cost                    0.0123
   :cwd                     "/tmp/project"
   :transcript              ["user> hi" "assistant> hello"]
   :spinner-frame           "⠋"
   :editor                  nil
   :modal                   nil
   :model-picker-index      0
   :model-picker-scroll     0
   :model-picker-editor     {:id :model-picker-query :editor nil}
   :model-picker-candidates []})

(deftest spinner-row-is-reserved-when-idle-test
  (let [svg (bramble/render-svg (sut/root-view base-state) 80 20)]
    (is (str/includes? svg "Idle"))
    (is (str/includes? svg "assistant>&#160;hello"))))

(deftest empty-transcript-renders-placeholder-test
  (let [svg (bramble/render-svg (sut/root-view (assoc base-state :transcript [])) 80 20)]
    (is (str/includes? svg "No&#160;transcript&#160;yet."))))

(deftest status-line-includes-model-provider-and-accounting-test
  (let [[cwd-line status-line] (sut/status-lines (assoc base-state :cwd "/home/ramblurr/src/github.com/outskirtslabs/llx/examples/bramble-coding-agent"))]
    (is (= "cwd: ~/src/github.com/outskirtslabs/llx/examples/bramble-coding-agent"
           cwd-line))
    (is (str/includes? status-line "gpt-test"))
    (is (str/includes? status-line "openai"))
    (is (str/includes? status-line "3 turns"))
    (is (str/includes? status-line "1.2k tokens"))
    (is (str/includes? status-line "$0.0123"))
    (is (not (str/includes? status-line "/home/ramblurr")))))

(deftest transcript-and-composer-expand-to-fill-width-test
  (let [{:keys [compositor]} (bramble/build-frame (sut/root-view base-state) 80 20)
        transcript-widget    (compose/get-widget-by-id compositor :transcript-box)
        composer-widget      (compose/get-widget-by-id compositor :composer-box)
        transcript-geom      (compose/get-geometry compositor transcript-widget)
        composer-geom        (compose/get-geometry compositor composer-widget)]
    (is (= 78 (.width (:region transcript-geom))))
    (is (= 78 (.width (:region composer-geom))))))

(deftest transcript-view-omits-border-and-side-padding-test
  (let [[tag attrs child] (#'sut/transcript-view base-state)]
    (is (= :box tag))
    (is (= :transcript-box (:id attrs)))
    (is (= Integer/MAX_VALUE (:width attrs)))
    (is (nil? (:border attrs)))
    (is (= :vstack (first child)))))

(deftest transcript-lines-wrap-to-available-width-test
  (is (= ["assistant> Hello! How can I help"
          "you today? Whether you have a"
          "coding question, need help with"
          "a bug,"]
         (sut/wrap-transcript-lines
          ["assistant> Hello! How can I help you today? Whether you have a coding question, need help with a bug,"]
          32))))

(deftest model-picker-modal-renders-candidates-and-help-test
  (let [state (assoc base-state
                     :modal :model-picker
                     :model-picker-index 1
                     :model-picker-candidates [{:id "claude-sonnet" :provider :anthropic}
                                               {:id "gpt-test" :provider :openai}])
        svg   (bramble/render-svg (sut/root-view state) 80 20)]
    (is (str/includes? svg ">&#160;"))
    (is (str/includes? svg "claude-sonnet"))
    (is (str/includes? svg "gpt-test"))
    (is (str/includes? svg "PgUp"))
    (is (str/includes? svg "PgDn"))
    (is (str/includes? svg "Enter"))
    (is (str/includes? svg "Esc"))))

(deftest model-picker-page-size-uses-terminal-height-test
  (is (= 6 (sut/model-picker-page-size {:terminal-rows 14})))
  (is (= 1 (sut/model-picker-page-size {:terminal-rows 8}))))

(deftest model-picker-visible-candidates-follow-scroll-offset-test
  (let [candidates (mapv (fn [n] {:id (str "model-" n) :provider :openai}) (range 8))]
    (is (= ["model-2" "model-3" "model-4" "model-5" "model-6" "model-7"]
           (mapv :id
                 (sut/visible-model-picker-candidates
                  {:terminal-rows           14
                   :model-picker-scroll     2
                   :model-picker-candidates candidates}))))))

(deftest filtered-model-picker-candidates-match-id-provider-and-name-test
  (let [candidates [{:id "claude-sonnet-4-6" :name "Claude Sonnet 4.6" :provider :anthropic}
                    {:id "gpt-5-mini" :name "GPT-5 Mini" :provider :openai}
                    {:id "claude-haiku-4-5" :name "Claude Haiku 4.5" :provider :anthropic}]]
    (is (= ["claude-sonnet-4-6"]
           (mapv :id
                 (sut/filtered-model-picker-candidates
                  {:model-picker-editor     {:id :model-picker-query :editor nil}
                   :model-picker-query      "sonnet 4.6"
                   :model-picker-candidates candidates}))))
    (is (= ["gpt-5-mini"]
           (mapv :id
                 (sut/filtered-model-picker-candidates
                  {:model-picker-query      "openai"
                   :model-picker-candidates candidates}))))))

(deftest model-picker-modal-renders-filter-prompt-details-and-count-test
  (let [state (assoc base-state
                     :modal :model-picker
                     :model-picker-query "claude"
                     :model-picker-index 0
                     :model-picker-scroll 0
                     :model-picker-candidates [{:id "claude-sonnet-4-6" :name "Claude Sonnet 4.6" :provider :anthropic}
                                               {:id "claude-haiku-4-5" :name "Claude Haiku 4.5" :provider :anthropic}
                                               {:id "gpt-5-mini" :name "GPT-5 Mini" :provider :openai}])
        svg   (bramble/render-svg (sut/root-view state) 80 20)]
    (is (str/includes? svg ">&#160;"))
    (is (str/includes? svg "claude"))
    (is (str/includes? svg "claude-sonnet-4-6"))
    (is (not (str/includes? svg "gpt-5-mini")))
    (is (str/includes? svg "(1/2)"))
    (is (str/includes? svg "Model&#160;Name:&#160;Claude&#160;Sonnet&#160;4.6"))))

(deftest model-picker-modal-renders-empty-filter-state-test
  (let [state (assoc base-state
                     :modal :model-picker
                     :model-picker-query "missing"
                     :model-picker-candidates [{:id "claude-sonnet-4-6" :name "Claude Sonnet 4.6" :provider :anthropic}])
        svg   (bramble/render-svg (sut/root-view state) 80 20)]
    (is (str/includes? svg "No&#160;matching&#160;models"))
    (is (str/includes? svg "(0/0)"))))
