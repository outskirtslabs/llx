(ns llx-ai.live.openai-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]))

(def live-model
  {:id             (or (System/getenv "LLX_LIVE_OPENAI_MODEL") "")
   :name           "Live OpenAI model"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(deftest live-openai-complete
  (let [run-live? (= "1" (System/getenv "LLX_RUN_LIVE_TESTS"))
        api-key   (System/getenv "OPENAI_API_KEY")
        model-id  (System/getenv "LLX_LIVE_OPENAI_MODEL")]
    (if (and run-live? api-key model-id)
      (testing "real OpenAI call returns canonical assistant response"
        (let [out (client/complete live-model
                                   {:messages [{:role      :user
                                                :content   "reply with exactly: llx smoke ok"
                                                :timestamp 1}]}
                                   {:api-key           api-key
                                    :max-output-tokens 64})]
          (is (= :assistant (:role out)))
          (is (#{:stop :length} (:stop-reason out)))
          (is (seq (:content out)))))
      (is true "skipped live OpenAI test; set LLX_RUN_LIVE_TESTS=1, OPENAI_API_KEY, LLX_LIVE_OPENAI_MODEL"))))
