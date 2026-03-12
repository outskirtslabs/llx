(ns bramble-coding-agent.main-test
  (:require
   [bramble-coding-agent.main :as sut]
   [bramble-coding-agent.ui.editor :as editor]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [ol.llx.agent :as agent]
   [ol.llx.ai :as ai]
   [ol.tendril.event.key :as key]
   [town.lilac.flex :as flex]))

(defn- key-event
  [& {:keys [kind code value modifiers]
      :or   {kind      ::key/press
             modifiers #{}}}]
  {::key/kind      kind
   ::key/code      code
   ::key/value     value
   ::key/modifiers modifiers})

(deftest parse-args-with-model-flag-test
  (is (= {:model-id "claude-3-5-sonnet-20241022"}
         (sut/parse-args ["--model" "claude-3-5-sonnet-20241022"]))))

(deftest parse-args-no-args-returns-error-test
  (let [r (sut/parse-args [])]
    (is (string? (:error r)))
    (is (str/includes? (:error r) "Usage"))))

(deftest resolve-model-unknown-id-returns-error-test
  (with-redefs [ai/get-providers (constantly [:anthropic])
                ai/get-models    (constantly [{:id "known" :provider :anthropic}])]
    (let [r (sut/resolve-model "missing")]
      (is (string? (:error r)))
      (is (str/includes? (:error r) "Unknown")))))

(deftest resolve-model-ambiguous-id-returns-error-test
  (with-redefs [ai/get-providers (constantly [:openai :openai-codex])
                ai/get-models    (fn [provider]
                                   [{:id "dup" :provider provider}])]
    (let [r (sut/resolve-model "dup")]
      (is (string? (:error r)))
      (is (str/includes? (:error r) "Ambiguous")))))

(deftest collect-model-candidates-sorts-by-provider-then-id-test
  (with-redefs [ai/get-providers (constantly [:openai :anthropic])
                ai/get-models    (fn [provider]
                                   (case provider
                                     :openai [{:id "zeta" :provider :openai}]
                                     :anthropic [{:id "alpha" :provider :anthropic}
                                                 {:id "beta" :provider :anthropic}]))]
    (is (= [{:id "alpha" :provider :anthropic}
            {:id "beta" :provider :anthropic}
            {:id "zeta" :provider :openai}]
           (sut/collect-model-candidates)))))

(deftest compute-accounting-sums-usage-from-assistant-messages-test
  (let [messages [{:role  :assistant
                   :usage {:input 10             :output 20 :total-tokens 30
                           :cost  {:total 0.001}}}
                  {:role  :assistant
                   :usage {:input 5              :output 15 :total-tokens 20
                           :cost  {:total 0.002}}}
                  {:role :user}]
        r        (sut/compute-accounting messages)]
    (is (= 2 (:turns r)))
    (is (= 15 (get-in r [:tokens :input])))
    (is (= 35 (get-in r [:tokens :output])))
    (is (= 50 (get-in r [:tokens :total-tokens])))
    (is (< (Math/abs (- 0.003 (:cost r))) 1e-9))))

(deftest handle-submission-routes-slash-commands-test
  (let [calls (atom [])
        state (atom (sut/initial-ui-state {:id "model-a" :provider :anthropic}))]
    (sut/handle-submission!
     {:state_            state
      :agent             ::agent
      :exit!             #(swap! calls conj [:exit])
      :prompt!           (fn [_ messages] (swap! calls conj [:prompt messages]))
      :reset!            (fn [_] (swap! calls conj [:reset]))
      :set-model-picker! #(swap! calls conj [:open-model-picker])
      :close-agent!      (fn [_] (swap! calls conj [:close]))}
     "/clear")
    (sut/handle-submission!
     {:state_            state
      :agent             ::agent
      :exit!             #(swap! calls conj [:exit])
      :prompt!           (fn [_ messages] (swap! calls conj [:prompt messages]))
      :reset!            (fn [_] (swap! calls conj [:reset]))
      :set-model-picker! #(swap! calls conj [:open-model-picker])
      :close-agent!      (fn [_] (swap! calls conj [:close]))}
     "/model")
    (sut/handle-submission!
     {:state_            state
      :agent             ::agent
      :exit!             #(swap! calls conj [:exit])
      :prompt!           (fn [_ messages] (swap! calls conj [:prompt messages]))
      :reset!            (fn [_] (swap! calls conj [:reset]))
      :set-model-picker! #(swap! calls conj [:open-model-picker])
      :close-agent!      (fn [_] (swap! calls conj [:close]))}
     "/quit")
    (is (= [[:reset]
            [:open-model-picker]
            [:close]
            [:exit]]
           @calls))))

(deftest handle-submission-prompts-agent-for-normal-text-test
  (let [calls (atom [])]
    (sut/handle-submission!
     {:state_            (atom (sut/initial-ui-state {:id "model-a" :provider :anthropic}))
      :agent             ::agent
      :exit!             (fn [])
      :prompt!           (fn [_ messages] (swap! calls conj messages))
      :reset!            (fn [_])
      :set-model-picker! (fn [])
      :close-agent!      (fn [_])}
     "explain the repo")
    (let [[message] (first @calls)]
      (is (= :user (:role message)))
      (is (= "explain the repo" (:content message)))
      (is (int? (:timestamp message))))))

(deftest handle-submission-clear-works-with-flex-source-test
  (let [state (flex/source (assoc (sut/initial-ui-state {:id "model-a" :provider :anthropic})
                                  :transcript ["user> hi"]
                                  :editor (editor/init :composer "draft")))]
    (sut/handle-submission!
     {:state_            state
      :agent             ::agent
      :exit!             (fn [])
      :prompt!           (fn [_ _])
      :reset!            (fn [_])
      :set-model-picker! (fn [])
      :close-agent!      (fn [_])}
     "/clear")
    (is (= [] (:transcript @state)))
    (is (= "" (editor/text (:editor @state))))))

(deftest model-picker-helpers-work-with-flex-source-test
  (let [models [{:id "claude-sonnet" :provider :anthropic}
                {:id "gpt-test" :provider :openai}]
        state  (flex/source (sut/initial-ui-state {:id "gpt-test" :provider :openai}))
        calls  (atom [])]
    (with-redefs [ai/get-providers (constantly [:openai :anthropic])
                  ai/get-models    (fn [provider]
                                     (case provider
                                       :anthropic [(first models)]
                                       :openai [(second models)]))
                  agent/set-model  (fn [_ model] (swap! calls conj model))]
      (#'sut/open-model-picker! state)
      (is (= :model-picker (:modal @state)))
      (is (= 1 (:model-picker-index @state)))
      (is (= 0 (:model-picker-scroll @state)))
      (is (= models (:model-picker-candidates @state)))

      (#'sut/confirm-model-picker! state ::agent)
      (is (= [(second models)] @calls))
      (is (= {:id "gpt-test" :provider :openai} (:current-model @state)))
      (is (nil? (:modal @state)))
      (is (= "system> model set to gpt-test [openai]"
             (last (:transcript @state))))

      (#'sut/open-model-picker! state)
      (#'sut/close-model-picker! state)
      (is (nil? (:modal @state))))))

(deftest open-model-picker-scrolls-current-selection-into-view-test
  (let [models (mapv (fn [n]
                       {:id       (str "model-" n)
                        :provider :openai})
                     (range 10))
        state  (flex/source (assoc (sut/initial-ui-state (nth models 8))
                                   :terminal-rows 14))]
    (with-redefs [ai/get-providers (constantly [:openai])
                  ai/get-models    (fn [provider]
                                     (->> models
                                          (filter #(= provider (:provider %)))
                                          vec))]
      (#'sut/open-model-picker! state)
      (is (= 8 (:model-picker-index @state)))
      (is (= 3 (:model-picker-scroll @state))))))

(deftest model-picker-down-arrow-scrolls-when-selection-leaves-page-test
  (let [state (atom {:modal                   :model-picker
                     :terminal-rows           14
                     :model-picker-index      5
                     :model-picker-scroll     0
                     :model-picker-candidates (mapv (fn [n] {:id (str "model-" n) :provider :openai})
                                                    (range 10))})]
    (#'sut/handle-model-picker-event! state
                                      ::agent
                                      (key-event :code ::key/down))
    (is (= 6 (:model-picker-index @state)))
    (is (= 1 (:model-picker-scroll @state)))))

(deftest model-picker-page-up-and-down-jump-by-visible-page-test
  (let [state (atom {:modal                   :model-picker
                     :terminal-rows           14
                     :model-picker-index      1
                     :model-picker-scroll     0
                     :model-picker-candidates (mapv (fn [n] {:id (str "model-" n) :provider :openai})
                                                    (range 12))})]
    (#'sut/handle-model-picker-event! state
                                      ::agent
                                      (key-event :code ::key/page-down))
    (is (= 7 (:model-picker-index @state)))
    (is (= 6 (:model-picker-scroll @state)))

    (#'sut/handle-model-picker-event! state
                                      ::agent
                                      (key-event :code ::key/page-up))
    (is (= 1 (:model-picker-index @state)))
    (is (= 0 (:model-picker-scroll @state)))))

(deftest model-picker-query-editing-resets-selection-to-filtered-list-test
  (let [state (atom {:modal                   :model-picker
                     :terminal-rows           8
                     :model-picker-index      5
                     :model-picker-scroll     4
                     :model-picker-query      ""
                     :model-picker-editor     (editor/init :model-picker-query "")
                     :model-picker-candidates [{:id "claude-sonnet-4-6" :name "Claude Sonnet 4.6" :provider :anthropic}
                                               {:id "claude-haiku-4-5" :name "Claude Haiku 4.5" :provider :anthropic}
                                               {:id "gpt-5-mini" :name "GPT-5 Mini" :provider :openai}]})]
    (#'sut/handle-model-picker-event! state
                                      ::agent
                                      (key-event :value \h))
    (is (= "h" (:model-picker-query @state)))
    (is (= 0 (:model-picker-index @state)))
    (is (= 0 (:model-picker-scroll @state)))))

(deftest model-picker-escape-aborts-session-test
  (let [state (atom {:modal               :model-picker
                     :model-picker-query  "sonnet"
                     :model-picker-editor (editor/init :model-picker-query "sonnet")})]
    (#'sut/handle-model-picker-event! state
                                      ::agent
                                      (key-event :code ::key/escape))
    (is (nil? (:modal @state)))))

(deftest apply-agent-event-updates-transcript-and-accounting-test
  (let [user-message      {:role      :user
                           :content   "show me the repo"
                           :timestamp 1}
        assistant-message {:role    :assistant
                           :content [{:type :text :text "hello\nworld"}]
                           :usage   {:input 1              :output 2 :total-tokens 3
                                     :cost  {:total 0.004}}}
        final-state       (reduce sut/apply-agent-event
                                  (sut/initial-ui-state {:id "gpt-test" :provider :openai})
                                  [{:type :ol.llx.agent.event/agent-start}
                                   {:type    :ol.llx.agent.event/message-start
                                    :message user-message}
                                   {:type    :ol.llx.agent.event/message-end
                                    :message user-message}
                                   {:type    :ol.llx.agent.event/message-start
                                    :message {:role :assistant}}
                                   {:type  :ol.llx.agent.event/message-update
                                    :chunk {:content [{:type :text :text "hello\nworld"}]}}
                                   {:type      :ol.llx.agent.event/tool-execution-start
                                    :tool-name "bash"}
                                   {:type      :ol.llx.agent.event/tool-execution-end
                                    :tool-name "bash"
                                    :is-error? false}
                                   {:type    :ol.llx.agent.event/message-end
                                    :message assistant-message}
                                   {:type     :ol.llx.agent.event/agent-end
                                    :messages [user-message assistant-message]}])]
    (is (false? (:working? final-state)))
    (is (= ["user> show me the repo"
            "tool> bash ..."
            "tool> bash ok"
            "assistant> hello"
            "world"]
           (:transcript final-state)))
    (is (= 1 (:turns final-state)))
    (is (= 3 (get-in final-state [:tokens :total-tokens])))
    (is (= 0.004 (:cost final-state)))))

(deftest ctrl-c-first-tap-clears-editor-and-arms-exit-test
  (let [state  (assoc (sut/initial-ui-state {:id "gpt-test" :provider :openai})
                      :editor {:buffer "placeholder"})
        result (sut/apply-global-shortcut state
                                          (key-event :value \c
                                                     :modifiers #{::key/control})
                                          1000)]
    (is (= :clear-editor (:effect result)))
    (is (= 1000 (:pending-exit-shortcut-at (:state result))))))

(deftest ctrl-c-double-tap-within-threshold-exits-test
  (let [state1  (:state (sut/apply-global-shortcut
                         (sut/initial-ui-state {:id "gpt-test" :provider :openai})
                         (key-event :value \c
                                    :modifiers #{::key/control})
                         1000))
        result2 (sut/apply-global-shortcut
                 state1
                 (key-event :value \c
                            :modifiers #{::key/control})
                 1999)]
    (is (= :exit (:effect result2)))
    (is (nil? (:pending-exit-shortcut-at (:state result2))))))

(deftest ctrl-c-after-threshold-rearms-instead-of-exiting-test
  (let [state1  (:state (sut/apply-global-shortcut
                         (sut/initial-ui-state {:id "gpt-test" :provider :openai})
                         (key-event :value \c
                                    :modifiers #{::key/control})
                         1000))
        result2 (sut/apply-global-shortcut
                 state1
                 (key-event :value \c
                            :modifiers #{::key/control})
                 2001)]
    (is (= :clear-editor (:effect result2)))
    (is (= 2001 (:pending-exit-shortcut-at (:state result2))))))

(deftest handle-app-event-ctrl-c-clears-flex-source-editor-test
  (let [state   (flex/source (assoc (sut/initial-ui-state {:id "gpt-test" :provider :openai})
                                    :editor (editor/init :composer "draft")))
        exited? (atom false)]
    (#'sut/handle-app-event state ::agent #(reset! exited? true)
                            (key-event :value \c
                                       :modifiers #{::key/control}))
    (is (false? @exited?))
    (is (= "" (editor/text (:editor @state))))))
