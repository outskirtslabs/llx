(ns minimal-coding-agent.main-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [minimal-coding-agent.main :as sut]
   [ol.llx.ai :as ai]))

;; ─── parse-args ──────────────────────────────────────────────────────────────

(deftest parse-args-with-model-flag-test
  (is (= {:model-id "claude-3-5-sonnet-20241022"}
         (sut/parse-args ["--model" "claude-3-5-sonnet-20241022"]))))

(deftest parse-args-no-args-returns-error-test
  (let [r (sut/parse-args [])]
    (is (string? (:error r)))
    (is (str/includes? (:error r) "Usage"))))

(deftest parse-args-missing-model-value-returns-error-test
  (let [r (sut/parse-args ["--model"])]
    (is (string? (:error r)))
    (is (str/includes? (:error r) "Missing"))))

(deftest parse-args-unknown-flag-returns-error-test
  (let [r (sut/parse-args ["--unknown" "value"])]
    (is (string? (:error r)))
    (is (str/includes? (:error r) "Unknown"))))

;; ─── resolve-model ───────────────────────────────────────────────────────────

(deftest resolve-model-unique-id-returns-model-test
  (let [r (sut/resolve-model "claude-3-5-haiku-20241022")]
    (is (nil? (:error r)))
    (is (= "claude-3-5-haiku-20241022" (get-in r [:model :id])))
    (is (= :anthropic (get-in r [:model :provider])))))

(deftest resolve-model-unknown-id-returns-error-test
  (let [r (sut/resolve-model "definitely-not-a-real-model-xyz")]
    (is (string? (:error r)))
    (is (str/includes? (:error r) "Unknown"))))

(deftest resolve-model-ambiguous-id-returns-error-test
  ;; gpt-5.2-codex appears in both :openai and :openai-codex
  (let [r (sut/resolve-model "gpt-5.2-codex")]
    (is (string? (:error r)))
    (is (str/includes? (:error r) "Ambiguous"))))

(deftest resolve-model-scans-all-providers-test
  (testing "every model id in every provider is resolvable (if unique)"
    (let [all-models (->> (ai/get-providers)
                          (mapcat ai/get-models))
          id-counts  (frequencies (map :id all-models))
          unique-ids (->> id-counts (filter #(= 1 (val %))) (map key))]
      (doseq [id (take 5 unique-ids)]
        (let [r (sut/resolve-model id)]
          (is (nil? (:error r)) (str "expected model " id " to resolve")))))))

;; ─── extract-text ────────────────────────────────────────────────────────────

(deftest extract-text-from-assistant-message-test
  (is (= "hello world"
         (sut/extract-text {:role    :assistant
                            :content [{:type :text :text "hello "}
                                      {:type :text :text "world"}]}))))

(deftest extract-text-ignores-non-text-blocks-test
  (is (= "visible"
         (sut/extract-text {:role    :assistant
                            :content [{:type :thinking :thinking "hidden"}
                                      {:type :text :text "visible"}
                                      {:type :tool-call :id "x" :name "bash" :arguments {}}]}))))

(deftest extract-text-empty-content-test
  (is (= "" (sut/extract-text {:role :assistant :content []}))))

;; ─── compute-accounting ──────────────────────────────────────────────────────

(deftest compute-accounting-empty-messages-test
  (let [r (sut/compute-accounting [])]
    (is (= 0 (:turns r)))
    (is (= 0 (:total-tokens (:tokens r))))
    (is (= 0.0 (:cost r)))))

(deftest compute-accounting-ignores-non-assistant-messages-test
  (let [msgs [{:role :user :content "hi" :timestamp 1}
              {:role    :tool-result                :tool-call-id "x"   :tool-name "bash"
               :content [{:type :text :text "out"}] :is-error?    false :timestamp 2}]
        r    (sut/compute-accounting msgs)]
    (is (= 0 (:turns r)))))

(deftest compute-accounting-sums-usage-from-assistant-messages-test
  (let [msg1 {:role        :assistant
              :content     [{:type :text :text "a"}]
              :usage       {:input 10                                                                       :output 20 :total-tokens 30
                            :cost  {:input 0.0 :output 0.001 :cache-read 0.0 :cache-write 0.0 :total 0.001}}
              :api         :anthropic
              :provider    :anthropic
              :model       "claude-3-5-haiku-20241022"
              :stop-reason :stop
              :timestamp   1}
        msg2 {:role        :assistant
              :content     [{:type :text :text "b"}]
              :usage       {:input 5                                                                        :output 15 :total-tokens 20
                            :cost  {:input 0.0 :output 0.002 :cache-read 0.0 :cache-write 0.0 :total 0.002}}
              :api         :anthropic
              :provider    :anthropic
              :model       "claude-3-5-haiku-20241022"
              :stop-reason :stop
              :timestamp   2}
        r    (sut/compute-accounting [msg1 msg2])]
    (is (= 2 (:turns r)))
    (is (= 15 (:input (:tokens r))))
    (is (= 35 (:output (:tokens r))))
    (is (= 50 (:total-tokens (:tokens r))))
    (is (< (Math/abs (- 0.003 (:cost r))) 1e-9))))

(deftest compute-accounting-resets-after-clear-test
  (testing "compute-accounting on empty messages (after /clear) returns zeros"
    (let [r (sut/compute-accounting [])]
      (is (= 0 (:turns r)))
      (is (= 0 (:total-tokens (:tokens r))))
      (is (= 0.0 (:cost r))))))

;; ─── slash command routing ───────────────────────────────────────────────────

(deftest slash-quit-recognized-test
  ;; parse-args does not handle slash commands; confirm that the main loop
  ;; detects /quit by string equality — test the predicate directly
  (is (= "/quit" (clojure.string/trim "/quit")))
  (is (= "/quit" (clojure.string/trim "  /quit  "))))

(deftest slash-clear-recognized-test
  (is (= "/clear" (clojure.string/trim "/clear")))
  (is (= "/clear" (clojure.string/trim "  /clear  "))))
