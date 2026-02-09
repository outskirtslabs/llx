(ns llx-ai.live.anthropic-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.live.env :as live-env]))

(def live-model
  {:id             (or (live-env/get-env "LLX_LIVE_ANTHROPIC_MODEL") "claude-sonnet-4-5")
   :name           "Live Anthropic model"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       (or (live-env/get-env "LLX_LIVE_ANTHROPIC_BASE_URL") "https://api.anthropic.com")
   :context-window 200000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(deftest live-anthropic-complete
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (testing "real Anthropic call returns canonical assistant response"
      (let [out (client/complete live-model
                                 {:messages [{:role      :user
                                              :content   "reply with exactly: llx anthropic smoke ok"
                                              :timestamp 1}]}
                                 {:api-key           api-key
                                  :max-output-tokens 128})]
        (is (= :assistant (:role out)))
        (is (#{:stop :length :tool-use} (:stop-reason out)))
        (is (seq (:content out)))))))

(deftest live-anthropic-stream
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (testing "real Anthropic stream emits canonical events and terminal assistant result"
      (let [stream (client/stream live-model
                                  {:messages [{:role      :user
                                               :content   "reply with exactly: llx anthropic stream ok"
                                               :timestamp 1}]}
                                  {:api-key           api-key
                                   :max-output-tokens 128})
            events (event-stream/drain! stream)
            out    (event-stream/result stream)]
        (is (= :start (:type (first events))))
        (is (#{:done :error} (:type (last events))))
        (is (= :assistant (:role out)))
        (is (seq (:content out)))))))
