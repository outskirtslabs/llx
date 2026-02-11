(ns llx.ai.live.cross-provider-handoff-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx.ai.impl.client.jvm :as client]
   [llx.ai.live.env :as live-env]
   [llx.ai.live.models :as models]))

(set! *warn-on-reflection* true)

(defn- handoff-check!
  [model-a opts-a prompt-a model-b opts-b]
  (let [user1  {:role      :user
                :content   prompt-a
                :timestamp 1}
        step-1 (client/complete* model-a {:messages [user1]} opts-a)
        user2  {:role      :user
                :content   "Now say hi and mention the previous sentence briefly."
                :timestamp 2}
        step-2 (client/complete* model-b {:messages [user1 step-1 user2]} opts-b)]
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
        (handoff-check! models/openai-completions {:api-key openai-key :max-output-tokens 96}
                        "Give one short sentence about Clojure."
                        models/anthropic {:api-key anthropic-key :max-output-tokens 128}))))
  (testing "Anthropic -> OpenAI"
    (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
          anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
      (if-not (and openai-key anthropic-key)
        (is true "Skipping: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
        (handoff-check! models/anthropic {:api-key anthropic-key :max-output-tokens 96}
                        "Give one short sentence about Lisp history."
                        models/openai-completions {:api-key openai-key :max-output-tokens 128}))))
  (testing "OpenAI Responses -> Anthropic"
    (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
          anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
      (if-not (and openai-key anthropic-key)
        (is true "Skipping: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
        (handoff-check! models/openai-responses {:api-key           openai-key
                                                 :max-output-tokens 128
                                                 :reasoning         {:effort :high :summary :detailed}}
                        "Give one short sentence about Clojure macros."
                        models/anthropic {:api-key anthropic-key :max-output-tokens 128}))))
  (testing "Anthropic -> OpenAI Responses"
    (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
          anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
      (if-not (and openai-key anthropic-key)
        (is true "Skipping: OPENAI_API_KEY and ANTHROPIC_API_KEY required")
        (handoff-check! models/anthropic {:api-key anthropic-key :max-output-tokens 96}
                        "Give one short sentence about Lisp history."
                        models/openai-responses {:api-key           openai-key
                                                 :max-output-tokens 128
                                                 :reasoning         {:effort :high :summary :detailed}}))))
  (testing "OpenAI Responses -> OpenAI Completions"
    (let [openai-key (live-env/get-env "OPENAI_API_KEY")]
      (if-not openai-key
        (is true "Skipping: OPENAI_API_KEY required")
        (handoff-check! models/openai-responses {:api-key           openai-key
                                                 :max-output-tokens 128
                                                 :reasoning         {:effort :high :summary :detailed}}
                        "Give one short sentence about Clojure protocols."
                        models/openai-completions {:api-key openai-key :max-output-tokens 128}))))
  (testing "Anthropic -> Google"
    (let [anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")
          google-key    (live-env/get-env "GEMINI_API_KEY")]
      (if-not (and anthropic-key google-key)
        (is true "Skipping: ANTHROPIC_API_KEY and GEMINI_API_KEY required")
        (handoff-check! models/anthropic {:api-key anthropic-key :max-output-tokens 96}
                        "Give one short sentence about Clojure macros."
                        models/google {:api-key           google-key
                                       :max-output-tokens 128
                                       :reasoning         {:level :high :effort :high}}))))
  (testing "Google -> OpenAI Completions"
    (let [openai-key (live-env/get-env "OPENAI_API_KEY")
          google-key (live-env/get-env "GEMINI_API_KEY")]
      (if-not (and openai-key google-key)
        (is true "Skipping: GEMINI_API_KEY and OPENAI_API_KEY required")
        (handoff-check! models/google {:api-key           google-key
                                       :max-output-tokens 96
                                       :reasoning         {:level :high :effort :high}}
                        "Give one short sentence about Clojure protocols."
                        models/openai-completions {:api-key openai-key :max-output-tokens 128}))))
  (testing "Google -> Mistral"
    (let [google-key  (live-env/get-env "GEMINI_API_KEY")
          mistral-key (live-env/get-env "MISTRAL_API_KEY")]
      (if-not (and google-key mistral-key)
        (is true "Skipping: GEMINI_API_KEY and MISTRAL_API_KEY required")
        (handoff-check! models/google {:api-key           google-key
                                       :max-output-tokens 96
                                       :reasoning         {:level :high :effort :high}}
                        "Give one short sentence about Clojure protocols."
                        models/mistral {:api-key mistral-key :max-output-tokens 128}))))
  (testing "Mistral -> OpenAI-compatible"
    (let [mistral-key (live-env/get-env "MISTRAL_API_KEY")]
      (if-not mistral-key
        (is true "Skipping: MISTRAL_API_KEY required")
        (handoff-check! models/mistral {:api-key mistral-key :max-output-tokens 96}
                        "Give one short sentence about Clojure macros."
                        models/ollama {:max-output-tokens 128})))))
