(ns llx-ai.live.google-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.live.env :as live-env]))

(set! *warn-on-reflection* true)

(def live-model
  {:id             (or (live-env/get-env "LLX_LIVE_GOOGLE_MODEL") "gemini-2.5-flash")
   :name           "Live Google model"
   :provider       :google
   :api            :google-generative-ai
   :base-url       (or (live-env/get-env "LLX_LIVE_GOOGLE_BASE_URL") "https://generativelanguage.googleapis.com/v1beta")
   :context-window 1048576
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(deftest live-google-complete
  (let [api-key (live-env/get-env "GEMINI_API_KEY")]
    (if-not api-key
      (is true "Skipping live Google complete test: GEMINI_API_KEY not set")
      (testing "real Google call returns canonical assistant response"
        (let [out (client/complete live-model
                                   {:messages [{:role      :user
                                                :content   "reply with exactly: llx google smoke ok"
                                                :timestamp 1}]}
                                   {:api-key           api-key
                                    :max-output-tokens 96
                                    :reasoning         {:level :high :effort :high :summary :detailed}})]
          (is (= :assistant (:role out)))
          (is (#{:stop :length :tool-use} (:stop-reason out)))
          (is (seq (:content out))))))))

(deftest live-google-stream
  (let [api-key (live-env/get-env "GEMINI_API_KEY")]
    (if-not api-key
      (is true "Skipping live Google stream test: GEMINI_API_KEY not set")
      (testing "real Google stream emits canonical events and terminal assistant result"
        (let [stream (client/stream live-model
                                    {:messages [{:role      :user
                                                 :content   "reply with exactly: llx google stream ok"
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
