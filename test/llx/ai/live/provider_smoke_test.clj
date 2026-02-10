(ns ^{:kaocha/parallelize? true} llx.ai.live.provider-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx.ai.impl.client.jvm :as client]
   [llx.ai.stream :as stream]
   [llx.ai.live.env :as live-env]
   [llx.ai.live.models :as models]))

(set! *warn-on-reflection* true)

(defn- collect-stream!
  [st]
  (let [events* (atom [])
        result* (promise)
        close*  (promise)]
    (stream/consume! st
                     {:on-event  (fn [event]
                                   (swap! events* conj event))
                      :on-result (fn [assistant-message]
                                   (deliver result* assistant-message))
                      :on-close  (fn [_close-meta]
                                   (deliver close* true))})
    (deref close* 60000 false)
    (let [result (deref result* 1000 nil)]
      {:events @events*
       :result result})))

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
           (client/complete models/anthropic
                            {:messages [{:role      :user
                                         :content   "reply with exactly: llx anthropic smoke ok"
                                         :timestamp 1}]}
                            {:api-key           api-key
                             :max-output-tokens 128})))
        (testing "stream"
          (assert-stream-ok
           (client/stream models/anthropic
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
           (client/complete models/google
                            {:messages [{:role      :user
                                         :content   "reply with exactly: llx google smoke ok"
                                         :timestamp 1}]}
                            {:api-key           api-key
                             :max-output-tokens 96
                             :reasoning         {:level :high :effort :high :summary :detailed}})))
        (testing "stream"
          (assert-stream-ok
           (client/stream models/google
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
           (client/complete models/mistral
                            {:messages [{:role      :user
                                         :content   "reply with exactly: llx mistral smoke ok"
                                         :timestamp 1}]}
                            {:api-key           api-key
                             :max-output-tokens 96})))
        (testing "stream"
          (assert-stream-ok
           (client/stream models/mistral
                          {:messages [{:role      :user
                                       :content   "reply with exactly: llx mistral stream ok"
                                       :timestamp 1}]}
                          {:api-key           api-key
                           :max-output-tokens 96})))))))

(deftest live-ollama-smoke
  (testing "complete"
    (assert-complete-ok
     (client/complete models/ollama
                      {:messages [{:role      :user
                                   :content   "reply with exactly: llx ollama smoke ok"
                                   :timestamp 1}]}
                      {:max-output-tokens 64})))
  (testing "stream"
    (assert-stream-ok
     (client/stream models/ollama
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
         (client/complete models/openai-completions
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
           (client/complete models/openai-responses
                            {:messages [{:role      :user
                                         :content   "reply with exactly: llx openai responses smoke ok"
                                         :timestamp 1}]}
                            {:api-key           api-key
                             :max-output-tokens 96
                             :reasoning         {:level :high :effort :high :summary :detailed}})))
        (testing "stream"
          (assert-stream-ok
           (client/stream models/openai-responses
                          {:messages [{:role      :user
                                       :content   "reply with exactly: llx openai responses stream ok"
                                       :timestamp 1}]}
                          {:api-key           api-key
                           :max-output-tokens 96
                           :reasoning         {:level :high :effort :high :summary :detailed}})))))))
