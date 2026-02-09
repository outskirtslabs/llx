(ns llx-ai.live.openai-responses-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.live.env :as live-env]))

(def live-model
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_MODEL") "gpt-5-mini")
   :name           "Live OpenAI Responses model"
   :provider       :openai
   :api            :openai-responses
   :base-url       (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_BASE_URL") "https://api.openai.com/v1")
   :context-window 400000
   :max-tokens     128000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(deftest live-openai-responses-complete
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (if-not api-key
      (is true "Skipping live OpenAI Responses complete test: OPENAI_API_KEY not set")
      (testing "real OpenAI Responses call returns canonical assistant response"
        (let [out (client/complete live-model
                                   {:messages [{:role      :user
                                                :content   "reply with exactly: llx openai responses smoke ok"
                                                :timestamp 1}]}
                                   {:api-key           api-key
                                    :max-output-tokens 96
                                    :reasoning         {:level :high :effort :high :summary :detailed}})]
          (is (= :assistant (:role out)))
          (is (#{:stop :length :tool-use} (:stop-reason out)))
          (is (seq (:content out))))))))

(deftest live-openai-responses-stream
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (if-not api-key
      (is true "Skipping live OpenAI Responses stream test: OPENAI_API_KEY not set")
      (testing "real OpenAI Responses stream emits canonical events and terminal assistant result"
        (let [stream (client/stream live-model
                                    {:messages [{:role      :user
                                                 :content   "reply with exactly: llx openai responses stream ok"
                                                 :timestamp 1}]}
                                    {:api-key           api-key
                                     :max-output-tokens 96
                                     :reasoning         {:level :high :effort :high :summary :detailed}})
              events (event-stream/drain! stream)
              out    (event-stream/result stream)]
          (is (= :start (:type (first events))))
          (is (#{:done :error} (:type (last events))))
          (is (= :assistant (:role out)))
          (is (seq (:content out))))))))
