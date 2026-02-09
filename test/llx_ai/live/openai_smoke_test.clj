(ns llx-ai.live.openai-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.live.env :as live-env]))

(def live-model
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_MODEL") "gpt-5.2-2025-12-11")
   :name           "Live OpenAI model"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(deftest live-openai-complete
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (if (seq api-key)
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
      (is true "skipped live OpenAI test; set OPENAI_API_KEY (or provide it in .env)"))))
