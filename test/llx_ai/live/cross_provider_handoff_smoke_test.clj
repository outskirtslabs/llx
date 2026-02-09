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

(def google-model
  {:id             (or (live-env/get-env "LLX_LIVE_GOOGLE_MODEL") "gemini-2.5-flash")
   :name           "Live Google handoff model"
   :provider       :google
   :api            :google-generative-ai
   :base-url       (or (live-env/get-env "LLX_LIVE_GOOGLE_BASE_URL") "https://generativelanguage.googleapis.com/v1beta")
   :context-window 1048576
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(def mistral-model
  {:id             (or (live-env/get-env "LLX_LIVE_MISTRAL_MODEL") "devstral-medium-latest")
   :name           "Live Mistral handoff model"
   :provider       :mistral
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_MISTRAL_BASE_URL") "https://api.mistral.ai/v1")
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def openai-compatible-model
  {:id             (or (live-env/get-env "LLX_LIVE_OLLAMA_MODEL") "devstral-small-2:latest")
   :name           "Live OpenAI-compatible handoff model"
   :provider       :openai-compatible
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_OLLAMA_BASE_URL") "http://localhost:11434/v1")
   :context-window 32768
   :max-tokens     4096
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(defn- handoff-check!
  [model-a opts-a prompt-a model-b opts-b]
  (let [user1  {:role      :user
                :content   prompt-a
                :timestamp 1}
        step-1 (client/complete model-a {:messages [user1]} opts-a)
        user2  {:role      :user
                :content   "Now say hi and mention the previous sentence briefly."
                :timestamp 2}
        step-2 (client/complete model-b {:messages [user1 step-1 user2]} opts-b)]
    (is (not= :error (:stop-reason step-1)))
    (is (not= :error (:stop-reason step-2)))
    (is (seq (:content step-2)))
    (is (#{:stop :length :tool-use} (:stop-reason step-2)))))

(deftest live-cross-provider-handoff
  (testing "OpenAI -> Anthropic"
    (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
          anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
      (if-not (and openai-key anthropic-key)
        (is true "Skipping: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
        (handoff-check! openai-model {:api-key openai-key :max-output-tokens 96}
                        "Give one short sentence about Clojure."
                        anthropic-model {:api-key anthropic-key :max-output-tokens 128}))))
  (testing "Anthropic -> OpenAI"
    (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
          anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
      (if-not (and openai-key anthropic-key)
        (is true "Skipping: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
        (handoff-check! anthropic-model {:api-key anthropic-key :max-output-tokens 96}
                        "Give one short sentence about Lisp history."
                        openai-model {:api-key openai-key :max-output-tokens 128}))))
  (testing "OpenAI Responses -> Anthropic"
    (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
          anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
      (if-not (and openai-key anthropic-key)
        (is true "Skipping: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
        (handoff-check! openai-responses-model {:api-key           openai-key
                                                :max-output-tokens 128
                                                :reasoning         {:level :high :effort :high :summary :detailed}}
                        "Give one short sentence about Clojure macros."
                        anthropic-model {:api-key anthropic-key :max-output-tokens 128}))))
  (testing "Anthropic -> OpenAI Responses"
    (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
          anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
      (if-not (and openai-key anthropic-key)
        (is true "Skipping: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
        (handoff-check! anthropic-model {:api-key anthropic-key :max-output-tokens 96}
                        "Give one short sentence about Lisp history."
                        openai-responses-model {:api-key           openai-key
                                                :max-output-tokens 128
                                                :reasoning         {:level :high :effort :high :summary :detailed}}))))
  (testing "OpenAI Responses -> OpenAI Completions"
    (let [openai-key (live-env/get-env "OPENAI_API_KEY")]
      (if-not openai-key
        (is true "Skipping: OPENAI_API_KEY required")
        (handoff-check! openai-responses-model {:api-key           openai-key
                                                :max-output-tokens 128
                                                :reasoning         {:level :high :effort :high :summary :detailed}}
                        "Give one short sentence about Clojure protocols."
                        openai-model {:api-key openai-key :max-output-tokens 128}))))
  (testing "Anthropic -> Google"
    (let [anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")
          google-key    (live-env/get-env "GEMINI_API_KEY")]
      (if-not (and anthropic-key google-key)
        (is true "Skipping: ANTHROPIC_API_KEY and GEMINI_API_KEY required")
        (handoff-check! anthropic-model {:api-key anthropic-key :max-output-tokens 96}
                        "Give one short sentence about Clojure macros."
                        google-model {:api-key           google-key
                                      :max-output-tokens 128
                                      :reasoning         {:level :high :effort :high :summary :detailed}}))))
  (testing "Google -> OpenAI Completions"
    (let [openai-key (live-env/get-env "OPENAI_API_KEY")
          google-key (live-env/get-env "GEMINI_API_KEY")]
      (if-not (and openai-key google-key)
        (is true "Skipping: GEMINI_API_KEY and OPENAI_API_KEY required")
        (handoff-check! google-model {:api-key           google-key
                                      :max-output-tokens 96
                                      :reasoning         {:level :high :effort :high :summary :detailed}}
                        "Give one short sentence about Clojure protocols."
                        openai-model {:api-key openai-key :max-output-tokens 128}))))
  (testing "Google -> Mistral"
    (let [google-key  (live-env/get-env "GEMINI_API_KEY")
          mistral-key (live-env/get-env "MISTRAL_API_KEY")]
      (if-not (and google-key mistral-key)
        (is true "Skipping: GEMINI_API_KEY and MISTRAL_API_KEY required")
        (handoff-check! google-model {:api-key           google-key
                                      :max-output-tokens 96
                                      :reasoning         {:level :high :effort :high :summary :detailed}}
                        "Give one short sentence about Clojure protocols."
                        mistral-model {:api-key mistral-key :max-output-tokens 128}))))
  (testing "Mistral -> OpenAI-compatible"
    (let [mistral-key (live-env/get-env "MISTRAL_API_KEY")]
      (if-not mistral-key
        (is true "Skipping: MISTRAL_API_KEY required")
        (handoff-check! mistral-model {:api-key mistral-key :max-output-tokens 96}
                        "Give one short sentence about Clojure macros."
                        openai-compatible-model {:max-output-tokens 128
                                                 :temperature       0.0})))))
