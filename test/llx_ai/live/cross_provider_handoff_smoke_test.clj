(ns llx-ai.live.cross-provider-handoff-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.live.env :as live-env]))

(def openai-model
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_MODEL") "gpt-4o-mini")
   :name           "Live OpenAI handoff model"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def anthropic-model
  {:id             (or (live-env/get-env "LLX_LIVE_ANTHROPIC_MODEL") "claude-sonnet-4-5")
   :name           "Live Anthropic handoff model"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       (or (live-env/get-env "LLX_LIVE_ANTHROPIC_BASE_URL") "https://api.anthropic.com")
   :context-window 200000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(deftest live-handoff-openai-to-anthropic
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (testing "OpenAI-produced context can continue on Anthropic"
      (let [user1  {:role      :user
                    :content   "Give one short sentence about Clojure."
                    :timestamp 1}
            step-1 (client/complete openai-model {:messages [user1]}
                                    {:api-key           openai-key
                                     :max-output-tokens 96})
            user2  {:role      :user
                    :content   "Now say hi and mention the previous sentence briefly."
                    :timestamp 2}
            step-2 (client/complete anthropic-model {:messages [user1 step-1 user2]}
                                    {:api-key           anthropic-key
                                     :max-output-tokens 128})]
        (is (not= :error (:stop-reason step-1)))
        (is (not= :error (:stop-reason step-2)))
        (is (seq (:content step-2)))
        (is (#{:stop :length :tool-use} (:stop-reason step-2)))))))

(deftest live-handoff-anthropic-to-openai
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (testing "Anthropic-produced context can continue on OpenAI"
      (let [user1  {:role      :user
                    :content   "Give one short sentence about Lisp history."
                    :timestamp 1}
            step-1 (client/complete anthropic-model {:messages [user1]}
                                    {:api-key           anthropic-key
                                     :max-output-tokens 96})
            user2  {:role      :user
                    :content   "Now say hi and summarize your prior sentence."
                    :timestamp 2}
            step-2 (client/complete openai-model {:messages [user1 step-1 user2]}
                                    {:api-key           openai-key
                                     :max-output-tokens 128})]
        (is (not= :error (:stop-reason step-1)))
        (is (not= :error (:stop-reason step-2)))
        (is (seq (:content step-2)))
        (is (#{:stop :length :tool-use} (:stop-reason step-2)))))))
