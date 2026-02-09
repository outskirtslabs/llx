(ns llx-ai.live.ollama-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.client.jvm :as client]))

(def live-model
  {:id             (or (System/getenv "LLX_LIVE_OLLAMA_MODEL") "devstral-small-2:latest")
   :name           "Live Ollama model"
   :provider       :openai-compatible
   :api            :openai-completions
   :base-url       "http://localhost:11434/v1"
   :context-window 32768
   :max-tokens     4096
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(deftest live-ollama-complete
  (testing "real Ollama OpenAI-compatible call returns canonical assistant response"
    (let [out (client/complete live-model
                               {:messages [{:role      :user
                                            :content   "reply with exactly: llx ollama smoke ok"
                                            :timestamp 1}]}
                               {:max-output-tokens 64
                                :temperature       0.0})]
      (is (= :assistant (:role out)))
      (is (#{:stop :length :tool-use} (:stop-reason out)))
      (is (seq (:content out))))))

(deftest live-ollama-stream
  (testing "real Ollama OpenAI-compatible stream emits canonical events and terminal assistant result"
    (let [stream (client/stream live-model
                                {:messages [{:role      :user
                                             :content   "reply with exactly: llx ollama stream ok"
                                             :timestamp 1}]}
                                {:max-output-tokens 64
                                 :temperature       0.0})
          events (event-stream/drain! stream)
          out    (event-stream/result stream)]
      (is (= :start (:type (first events))))
      (is (#{:done :error} (:type (last events))))
      (is (= :assistant (:role out)))
      (is (seq (:content out))))))
