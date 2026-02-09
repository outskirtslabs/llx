(ns llx-ai.transform-messages-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [llx-ai.adapters.openai-completions :as openai-completions]
   [llx-ai.transform-messages :as sut]))

(def source-openai-model
  {:provider :openai
   :api      :openai-responses
   :id       "gpt-5-mini"})

(def same-openai-model
  {:provider :openai
   :api      :openai-responses
   :id       "gpt-5-mini"})

(def other-openai-model
  {:provider :openai
   :api      :openai-responses
   :id       "gpt-5.2-codex"})

(def anthropic-model
  {:provider :anthropic
   :api      :anthropic-messages
   :id       "claude-sonnet-4-5"})

(defn- fixture
  [name]
  (-> (str "test/llx_ai/fixtures/handoff/" name ".edn")
      slurp
      edn/read-string))

(deftest same-model-replay-preserves-thinking-and-signatures
  (let [messages [{:role        :assistant
                   :content     [{:type :thinking :thinking "" :signature "enc-sig"}
                                 {:type :text :text "ok"}
                                 {:type      :tool-call
                                  :id        "call_1"
                                  :name      "echo"
                                  :arguments {:message "hello"}
                                  :signature "tc-sig"}]
                   :api         :openai-responses
                   :provider    :openai
                   :model       "gpt-5-mini"
                   :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                 :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                   :stop-reason :stop
                   :timestamp   1}]
        out      (sut/for-target-model messages same-openai-model {:clock/now-ms (constantly 9999)})]
    (is (= messages out))))

(deftest cross-model-replay-converts-thinking-and-drops-signatures
  (let [messages [{:role        :assistant
                   :content     [{:type :text :text "prefix"}
                                 {:type :thinking :thinking "hidden chain" :signature "enc-sig"}
                                 {:type      :tool-call
                                  :id        "call_2"
                                  :name      "echo"
                                  :arguments {:message "hello"}
                                  :signature "tc-sig"}]
                   :api         :openai-responses
                   :provider    :openai
                   :model       "gpt-5-mini"
                   :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                 :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                   :stop-reason :stop
                   :timestamp   1}]
        out      (sut/for-target-model messages other-openai-model {:clock/now-ms (constantly 9999)})]
    (is (= [{:role        :assistant
             :content     [{:type :text :text "prefix"}
                           {:type :text :text "hidden chain"}
                           {:type :tool-call :id "call_2" :name "echo" :arguments {:message "hello"}}]
             :api         :openai-responses
             :provider    :openai
             :model       "gpt-5-mini"
             :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                           :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
             :stop-reason :stop
             :timestamp   1}]
           out))))

(deftest tool-call-id-normalization-propagates-to-tool-result
  (let [messages  (fixture "pipe_id_prefilled")
        source-id (get-in messages [1 :content 0 :id])
        expected  (openai-completions/normalize-tool-call-id
                   source-id
                   {:provider :mistral :api :openai-completions :id "devstral-medium-latest"}
                   (second messages))
        out       (sut/for-target-model
                   messages
                   {:provider :mistral :api :openai-completions :id "devstral-medium-latest"}
                   {:clock/now-ms           (constantly 9999)
                    :normalize-tool-call-id openai-completions/normalize-tool-call-id})
        assistant (second out)
        result    (nth out 2)]
    (is (= expected (get-in assistant [:content 0 :id])))
    (is (= expected (:tool-call-id result)))
    (is (re-matches #"[A-Za-z0-9]{9}" expected))))

(deftest missing-tool-results-get-synthetic-error-message
  (let [out      (sut/for-target-model
                  (fixture "orphan_tool_call")
                  anthropic-model
                  {:clock/now-ms (constantly 4444)})
        inserted (nth out 2)]
    (is (= :tool-result (:role inserted)))
    (is (= "call_orphan" (:tool-call-id inserted)))
    (is (= "calculate" (:tool-name inserted)))
    (is (= [{:type :text :text "No result provided"}] (:content inserted)))
    (is (= true (:is-error? inserted)))
    (is (= 4444 (:timestamp inserted)))))

(deftest skipped-assistant-turns-with-error-or-aborted-stop-reasons
  (let [messages [{:role :user :content "q1" :timestamp 1}
                  {:role        :assistant
                   :content     [{:type :thinking :thinking "partial"}]
                   :api         :openai-responses
                   :provider    :openai
                   :model       "gpt-5-mini"
                   :usage       {:input 1                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 1
                                 :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                   :stop-reason :error
                   :timestamp   2}
                  {:role        :assistant
                   :content     [{:type :thinking :thinking "partial"}]
                   :api         :openai-responses
                   :provider    :openai
                   :model       "gpt-5-mini"
                   :usage       {:input 1                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 1
                                 :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                   :stop-reason :aborted
                   :timestamp   3}
                  {:role :user :content "q2" :timestamp 4}]
        out      (sut/for-target-model messages anthropic-model {:clock/now-ms (constantly 9999)})]
    (is (= [{:role :user :content "q1" :timestamp 1}
            {:role :user :content "q2" :timestamp 4}]
           out))))

(deftest user-interruption-inserts-only-missing-tool-results
  (let [messages [{:role :user :content "start" :timestamp 1}
                  {:role        :assistant
                   :content     [{:type :tool-call :id "call_a" :name "a" :arguments {}}
                                 {:type :tool-call :id "call_b" :name "b" :arguments {}}]
                   :api         :openai-responses
                   :provider    :openai
                   :model       "gpt-5-mini"
                   :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                 :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                   :stop-reason :tool-use
                   :timestamp   2}
                  {:role         :tool-result
                   :tool-call-id "call_a"
                   :tool-name    "a"
                   :content      [{:type :text :text "done"}]
                   :is-error?    false
                   :timestamp    3}
                  {:role :user :content "interrupt" :timestamp 4}]
        out      (sut/for-target-model messages anthropic-model {:clock/now-ms (constantly 7777)})
        inserted (nth out 3)]
    (is (= [:user :assistant :tool-result :tool-result :user] (mapv :role out)))
    (is (= "call_b" (:tool-call-id inserted)))
    (is (= 7777 (:timestamp inserted)))))

(deftest does-not-duplicate-existing-synthetic-tool-result
  (let [messages [{:role        :assistant
                   :content     [{:type :tool-call :id "call_existing" :name "echo" :arguments {}}]
                   :api         :openai-responses
                   :provider    :openai
                   :model       "gpt-5-mini"
                   :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                 :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                   :stop-reason :tool-use
                   :timestamp   1}
                  {:role         :tool-result
                   :tool-call-id "call_existing"
                   :tool-name    "echo"
                   :content      [{:type :text :text "No result provided"}]
                   :is-error?    true
                   :timestamp    2}
                  {:role :user :content "continue" :timestamp 3}]
        out      (sut/for-target-model messages anthropic-model {:clock/now-ms (constantly 9000)})]
    (is (= 1 (count (filter #(= :tool-result (:role %)) out))))))

(deftest fixture-for-aborted-reasoning-skips-aborted-turn
  (let [out (sut/for-target-model (fixture "aborted_reasoning")
                                  anthropic-model
                                  {:clock/now-ms (constantly 9001)})]
    (testing "aborted assistant turn is removed from replay"
      (is (= [:user :user] (mapv :role out))))))

(deftest synthetic-tool-result-uses-real-time-when-clock-missing
  (let [before-ms (System/currentTimeMillis)
        out       (sut/for-target-model
                   (fixture "orphan_tool_call")
                   anthropic-model
                   {})
        inserted  (nth out 2)
        after-ms  (System/currentTimeMillis)]
    (is (= :tool-result (:role inserted)))
    (is (<= before-ms (:timestamp inserted) after-ms))))
