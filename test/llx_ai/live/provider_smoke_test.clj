(ns llx-ai.live.provider-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.live.env :as live-env]))

(set! *warn-on-reflection* true)

(def anthropic-model
  {:id             (or (live-env/get-env "LLX_LIVE_ANTHROPIC_MODEL") "claude-sonnet-4-5")
   :name           "Live Anthropic model"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       (or (live-env/get-env "LLX_LIVE_ANTHROPIC_BASE_URL") "https://api.anthropic.com")
   :context-window 200000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(def google-model
  {:id             (or (live-env/get-env "LLX_LIVE_GOOGLE_MODEL") "gemini-2.5-flash")
   :name           "Live Google model"
   :provider       :google
   :api            :google-generative-ai
   :base-url       (or (live-env/get-env "LLX_LIVE_GOOGLE_BASE_URL") "https://generativelanguage.googleapis.com/v1beta")
   :context-window 1048576
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(def mistral-model
  {:id             (or (live-env/get-env "LLX_LIVE_MISTRAL_MODEL") "devstral-medium-latest")
   :name           "Live Mistral model"
   :provider       :mistral
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_MISTRAL_BASE_URL") "https://api.mistral.ai/v1")
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def ollama-model
  {:id             (or (live-env/get-env "LLX_LIVE_OLLAMA_MODEL") "devstral-small-2:latest")
   :name           "Live Ollama model"
   :provider       :openai-compatible
   :api            :openai-completions
   :base-url       "http://localhost:11434/v1"
   :context-window 32768
   :max-tokens     4096
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def openai-model
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_MODEL") "gpt-5.2-2025-12-11")
   :name           "Live OpenAI model"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def openai-responses-model
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_MODEL") "gpt-5-mini")
   :name           "Live OpenAI Responses model"
   :provider       :openai
   :api            :openai-responses
   :base-url       (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_BASE_URL") "https://api.openai.com/v1")
   :context-window 400000
   :max-tokens     128000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(defn- assert-complete-ok [out]
  (is (= :assistant (:role out)))
  (is (#{:stop :length :tool-use} (:stop-reason out)))
  (is (seq (:content out))))

(defn- assert-stream-ok [stream]
  (let [events (event-stream/drain! stream)
        out    (event-stream/result stream)]
    (is (= :start (:type (first events))))
    (is (#{:done :error} (:type (last events))))
    (is (= :assistant (:role out)))
    (is (seq (:content out)))))

(deftest live-anthropic-smoke
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (if-not api-key
      (is true "Skipping live Anthropic smoke test: ANTHROPIC_API_KEY not set")
      (do
        (testing "complete"
          (assert-complete-ok
           (client/complete anthropic-model
                            {:messages [{:role      :user
                                         :content   "reply with exactly: llx anthropic smoke ok"
                                         :timestamp 1}]}
                            {:api-key           api-key
                             :max-output-tokens 128})))
        (testing "stream"
          (assert-stream-ok
           (client/stream anthropic-model
                          {:messages [{:role      :user
                                       :content   "reply with exactly: llx anthropic stream ok"
                                       :timestamp 1}]}
                          {:api-key           api-key
                           :max-output-tokens 128})))))))

(deftest live-google-smoke
  (let [api-key (live-env/get-env "GEMINI_API_KEY")]
    (if-not api-key
      (is true "Skipping live Google smoke test: GEMINI_API_KEY not set")
      (do
        (testing "complete"
          (assert-complete-ok
           (client/complete google-model
                            {:messages [{:role      :user
                                         :content   "reply with exactly: llx google smoke ok"
                                         :timestamp 1}]}
                            {:api-key           api-key
                             :max-output-tokens 96
                             :reasoning         {:level :high :effort :high :summary :detailed}})))
        (testing "stream"
          (assert-stream-ok
           (client/stream google-model
                          {:messages [{:role      :user
                                       :content   "reply with exactly: llx google stream ok"
                                       :timestamp 1}]}
                          {:api-key           api-key
                           :max-output-tokens 96
                           :reasoning         {:level :high :effort :high :summary :detailed}})))))))

(deftest live-mistral-smoke
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")]
    (if-not api-key
      (is true "Skipping live Mistral smoke test: MISTRAL_API_KEY not set")
      (do
        (testing "complete"
          (assert-complete-ok
           (client/complete mistral-model
                            {:messages [{:role      :user
                                         :content   "reply with exactly: llx mistral smoke ok"
                                         :timestamp 1}]}
                            {:api-key           api-key
                             :max-output-tokens 96})))
        (testing "stream"
          (assert-stream-ok
           (client/stream mistral-model
                          {:messages [{:role      :user
                                       :content   "reply with exactly: llx mistral stream ok"
                                       :timestamp 1}]}
                          {:api-key           api-key
                           :max-output-tokens 96})))))))

(deftest live-ollama-smoke
  (testing "complete"
    (assert-complete-ok
     (client/complete ollama-model
                      {:messages [{:role      :user
                                   :content   "reply with exactly: llx ollama smoke ok"
                                   :timestamp 1}]}
                      {:max-output-tokens 64
                       :temperature       0.0})))
  (testing "stream"
    (assert-stream-ok
     (client/stream ollama-model
                    {:messages [{:role      :user
                                 :content   "reply with exactly: llx ollama stream ok"
                                 :timestamp 1}]}
                    {:max-output-tokens 64
                     :temperature       0.0}))))

(deftest live-openai-smoke
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (if-not api-key
      (is true "Skipping live OpenAI smoke test: OPENAI_API_KEY not set")
      (testing "complete"
        (assert-complete-ok
         (client/complete openai-model
                          {:messages [{:role      :user
                                       :content   "reply with exactly: llx smoke ok"
                                       :timestamp 1}]}
                          {:api-key           api-key
                           :max-output-tokens 64}))))))

(deftest live-openai-responses-smoke
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (if-not api-key
      (is true "Skipping live OpenAI Responses smoke test: OPENAI_API_KEY not set")
      (do
        (testing "complete"
          (assert-complete-ok
           (client/complete openai-responses-model
                            {:messages [{:role      :user
                                         :content   "reply with exactly: llx openai responses smoke ok"
                                         :timestamp 1}]}
                            {:api-key           api-key
                             :max-output-tokens 96
                             :reasoning         {:level :high :effort :high :summary :detailed}})))
        (testing "stream"
          (assert-stream-ok
           (client/stream openai-responses-model
                          {:messages [{:role      :user
                                       :content   "reply with exactly: llx openai responses stream ok"
                                       :timestamp 1}]}
                          {:api-key           api-key
                           :max-output-tokens 96
                           :reasoning         {:level :high :effort :high :summary :detailed}})))))))
