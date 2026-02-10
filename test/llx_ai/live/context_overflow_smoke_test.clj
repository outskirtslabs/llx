(ns llx-ai.live.context-overflow-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.live.env :as live-env]
   [llx-ai.live.models :as models]
   [llx-ai.utils.overflow :as overflow]))

(set! *warn-on-reflection* true)

(def ^:private lorem-ipsum
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. ")

(defn- generate-overflow-content
  [context-window]
  (let [target-tokens (+ context-window 10000)
        target-chars  (long (* target-tokens 4 1.5))
        repeats       (long (Math/ceil (/ target-chars (double (count lorem-ipsum)))))]
    (apply str (repeat repeats lorem-ipsum))))

(defn- test-context-overflow!
  [model opts]
  (let [oversized-text (generate-overflow-content (:context-window model))
        context        {:messages [{:role      :user
                                    :content   oversized-text
                                    :timestamp (System/currentTimeMillis)}]}
        result         (try
                         (client/complete model context opts)
                         (catch clojure.lang.ExceptionInfo e
                           {:role          :assistant
                            :content       []
                            :api           (:api model)
                            :provider      (:provider model)
                            :model         (:id model)
                            :usage         {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                                            :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                            :stop-reason   :error
                            :error-message (ex-message e)
                            :timestamp     (System/currentTimeMillis)}))]
    (is (overflow/context-overflow? result (:context-window model))
        (str "Expected context-overflow? to be true for " (:id model)
             " stop-reason=" (:stop-reason result)
             " error=" (:error-message result)))))

(deftest live-overflow-anthropic
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (testing "anthropic overflow detection"
      (test-context-overflow! models/anthropic {:api-key api-key :max-output-tokens 128}))))

(deftest live-overflow-openai-completions
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (testing "openai completions overflow detection"
      (test-context-overflow! models/openai-completions {:api-key api-key :max-output-tokens 128}))))

(deftest live-overflow-openai-responses
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (testing "openai responses overflow detection"
      (test-context-overflow! models/openai-responses {:api-key api-key :max-output-tokens 128}))))

(deftest live-overflow-google
  (let [api-key (live-env/get-env "GEMINI_API_KEY")]
    (testing "google overflow detection"
      (test-context-overflow! models/google {:api-key api-key :max-output-tokens 128}))))

(deftest live-overflow-mistral
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")]
    (testing "mistral overflow detection"
      (test-context-overflow! models/mistral {:api-key api-key :max-output-tokens 128}))))
