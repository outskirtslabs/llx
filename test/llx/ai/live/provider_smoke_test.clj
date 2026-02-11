(ns ^{:kaocha/parallelize? true} llx.ai.live.provider-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx.ai :as client]
   [llx.ai.live.env :as live-env]
   [llx.ai.live.models :as models]
   [llx.ai.test-util :as util]
   [llx.ai.impl.utils.await :as await]))

(set! *warn-on-reflection* true)

(def ^:private env
  (client/default-env))

(defn- collect-stream!
  [ch]
  (let [events (util/collect-channel-events! ch 60000)]
    {:events events
     :result (:assistant-message (last events))}))

(defn- assert-complete-ok [out]
  (is (= :assistant (:role out)))
  (is (#{:stop :length :tool-use} (:stop-reason out)))
  (is (seq (:content out))))

(defn- assert-stream-ok [stream]
  (let [{:keys [events result]} (collect-stream! stream)]
    (is (= :start (:type (first events))))
    (is (#{:done :error} (:type (last events))))
    (is (= :assistant (:role result)))
    (is (seq (:content result)))))

(deftest live-anthropic-smoke
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (if-not api-key
      (is true "Skipping live Anthropic smoke test: ANTHROPIC_API_KEY not set")
      (do
        (testing "complete"
          (assert-complete-ok
           (await/await! (client/complete* env models/anthropic
                                     {:messages [{:role      :user
                                                  :content   "reply with exactly: llx anthropic smoke ok"
                                                  :timestamp 1}]}
                                     {:api-key           api-key
                                      :max-output-tokens 128}))))
        (testing "stream"
          (assert-stream-ok
           (client/stream* env models/anthropic
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
           (await/await! (client/complete* env models/google
                                     {:messages [{:role      :user
                                                  :content   "reply with exactly: llx google smoke ok"
                                                  :timestamp 1}]}
                                     {:api-key           api-key
                                      :max-output-tokens 96
                                      :reasoning         {:level :high :effort :high}}))))
        (testing "stream"
          (assert-stream-ok
           (client/stream* env models/google
                           {:messages [{:role      :user
                                        :content   "reply with exactly: llx google stream ok"
                                        :timestamp 1}]}
                           {:api-key           api-key
                            :max-output-tokens 96
                            :reasoning         {:level :high :effort :high}})))))))

(deftest live-mistral-smoke
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")]
    (if-not api-key
      (is true "Skipping live Mistral smoke test: MISTRAL_API_KEY not set")
      (do
        (testing "complete"
          (assert-complete-ok
           (await/await! (client/complete* env models/mistral
                                     {:messages [{:role      :user
                                                  :content   "reply with exactly: llx mistral smoke ok"
                                                  :timestamp 1}]}
                                     {:api-key           api-key
                                      :max-output-tokens 96}))))
        (testing "stream"
          (assert-stream-ok
           (client/stream* env models/mistral
                           {:messages [{:role      :user
                                        :content   "reply with exactly: llx mistral stream ok"
                                        :timestamp 1}]}
                           {:api-key           api-key
                            :max-output-tokens 96})))))))

(deftest live-ollama-smoke
  (testing "complete"
    (assert-complete-ok
     (await/await! (client/complete* env models/ollama
                               {:messages [{:role      :user
                                            :content   "reply with exactly: llx ollama smoke ok"
                                            :timestamp 1}]}
                               {:max-output-tokens 64}))))
  (testing "stream"
    (assert-stream-ok
     (client/stream* env models/ollama
                     {:messages [{:role      :user
                                  :content   "reply with exactly: llx ollama stream ok"
                                  :timestamp 1}]}
                     {:max-output-tokens 64}))))

(deftest live-openai-smoke
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (if-not api-key
      (is true "Skipping live OpenAI smoke test: OPENAI_API_KEY not set")
      (testing "complete"
        (assert-complete-ok
         (await/await! (client/complete* env models/openai-completions
                                   {:messages [{:role      :user
                                                :content   "reply with exactly: llx smoke ok"
                                                :timestamp 1}]}
                                   {:api-key           api-key
                                    :max-output-tokens 64})))))))

(deftest live-openai-responses-smoke
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (if-not api-key
      (is true "Skipping live OpenAI Responses smoke test: OPENAI_API_KEY not set")
      (do
        (testing "complete"
          (assert-complete-ok
           (await/await! (client/complete* env models/openai-responses
                                     {:messages [{:role      :user
                                                  :content   "reply with exactly: llx openai responses smoke ok"
                                                  :timestamp 1}]}
                                     {:api-key           api-key
                                      :max-output-tokens 96
                                      :reasoning         {:effort :high :summary :detailed}}))))
        (testing "stream"
          (assert-stream-ok
           (client/stream* env models/openai-responses
                           {:messages [{:role      :user
                                        :content   "reply with exactly: llx openai responses stream ok"
                                        :timestamp 1}]}
                           {:api-key           api-key
                            :max-output-tokens 96
                            :reasoning         {:effort :high :summary :detailed}})))))))
