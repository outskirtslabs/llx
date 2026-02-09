(ns llx-ai.live.cross-provider-handoff-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.live.env :as live-env]))

(set! *warn-on-reflection* true)

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

(def openai-responses-model
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_MODEL") "gpt-5-mini")
   :name           "Live OpenAI Responses handoff model"
   :provider       :openai
   :api            :openai-responses
   :base-url       (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_BASE_URL") "https://api.openai.com/v1")
   :context-window 400000
   :max-tokens     128000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(deftest live-handoff-openai-to-anthropic
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (if-not (and openai-key anthropic-key)
      (is true "Skipping OpenAI->Anthropic handoff: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
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
          (is (#{:stop :length :tool-use} (:stop-reason step-2))))))))

(deftest live-handoff-anthropic-to-openai
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (if-not (and openai-key anthropic-key)
      (is true "Skipping Anthropic->OpenAI handoff: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
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
          (is (#{:stop :length :tool-use} (:stop-reason step-2))))))))

(deftest live-handoff-openai-responses-to-anthropic
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (if-not (and openai-key anthropic-key)
      (is true "Skipping Responses->Anthropic handoff: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
      (testing "OpenAI Responses-produced context can continue on Anthropic"
        (let [user1  {:role      :user
                      :content   "Give one short sentence about Clojure macros."
                      :timestamp 1}
              step-1 (client/complete openai-responses-model {:messages [user1]}
                                      {:api-key           openai-key
                                       :max-output-tokens 128
                                       :reasoning         {:level :high :effort :high :summary :detailed}})
              user2  {:role      :user
                      :content   "Now say hi and mention your previous sentence."
                      :timestamp 2}
              step-2 (client/complete anthropic-model {:messages [user1 step-1 user2]}
                                      {:api-key           anthropic-key
                                       :max-output-tokens 128})]
          (is (not= :error (:stop-reason step-1)))
          (is (not= :error (:stop-reason step-2)))
          (is (seq (:content step-2)))
          (is (#{:stop :length :tool-use} (:stop-reason step-2))))))))

(deftest live-handoff-anthropic-to-openai-responses
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (if-not (and openai-key anthropic-key)
      (is true "Skipping Anthropic->Responses handoff: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
      (testing "Anthropic-produced context can continue on OpenAI Responses"
        (let [user1  {:role      :user
                      :content   "Give one short sentence about Lisp history."
                      :timestamp 1}
              step-1 (client/complete anthropic-model {:messages [user1]}
                                      {:api-key           anthropic-key
                                       :max-output-tokens 96})
              user2  {:role      :user
                      :content   "Now say hi and summarize your prior sentence."
                      :timestamp 2}
              step-2 (client/complete openai-responses-model {:messages [user1 step-1 user2]}
                                      {:api-key           openai-key
                                       :max-output-tokens 128
                                       :reasoning         {:level :high :effort :high :summary :detailed}})]
          (is (not= :error (:stop-reason step-1)))
          (is (not= :error (:stop-reason step-2)))
          (is (seq (:content step-2)))
          (is (#{:stop :length :tool-use} (:stop-reason step-2))))))))

(deftest live-handoff-openai-responses-to-openai-completions
  (let [openai-key (live-env/get-env "OPENAI_API_KEY")]
    (if-not openai-key
      (is true "Skipping Responses->Completions handoff: OPENAI_API_KEY required")
      (testing "OpenAI Responses-produced context can continue on OpenAI Completions"
        (let [user1  {:role      :user
                      :content   "Give one short sentence about Clojure protocols."
                      :timestamp 1}
              step-1 (client/complete openai-responses-model {:messages [user1]}
                                      {:api-key           openai-key
                                       :max-output-tokens 128
                                       :reasoning         {:level :high :effort :high :summary :detailed}})
              user2  {:role      :user
                      :content   "Now say hi and summarize your prior sentence."
                      :timestamp 2}
              step-2 (client/complete openai-model {:messages [user1 step-1 user2]}
                                      {:api-key           openai-key
                                       :max-output-tokens 128})]
          (is (not= :error (:stop-reason step-1)))
          (is (not= :error (:stop-reason step-2)))
          (is (seq (:content step-2)))
          (is (#{:stop :length :tool-use} (:stop-reason step-2))))))))
