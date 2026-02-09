(ns llx-ai.live.mistral-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.live.env :as live-env]))

(set! *warn-on-reflection* true)

(def live-model
  {:id             (or (live-env/get-env "LLX_LIVE_MISTRAL_MODEL") "devstral-medium-latest")
   :name           "Live Mistral model"
   :provider       :mistral
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_MISTRAL_BASE_URL") "https://api.mistral.ai/v1")
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(deftest live-mistral-complete
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")]
    (if-not api-key
      (is true "Skipping live Mistral complete test: MISTRAL_API_KEY not set")
      (testing "real Mistral call returns canonical assistant response"
        (let [out (client/complete live-model
                                   {:messages [{:role      :user
                                                :content   "reply with exactly: llx mistral smoke ok"
                                                :timestamp 1}]}
                                   {:api-key           api-key
                                    :max-output-tokens 96})]
          (is (= :assistant (:role out)))
          (is (#{:stop :length :tool-use} (:stop-reason out)))
          (is (seq (:content out))))))))

(deftest live-mistral-stream
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")]
    (if-not api-key
      (is true "Skipping live Mistral stream test: MISTRAL_API_KEY not set")
      (testing "real Mistral stream emits canonical events and terminal assistant result"
        (let [stream (client/stream live-model
                                    {:messages [{:role      :user
                                                 :content   "reply with exactly: llx mistral stream ok"
                                                 :timestamp 1}]}
                                    {:api-key           api-key
                                     :max-output-tokens 96})
              events (event-stream/drain! stream)
              out    (event-stream/result stream)]
          (is (= :start (:type (first events))))
          (is (#{:done :error} (:type (last events))))
          (is (= :assistant (:role out)))
          (is (seq (:content out))))))))
