(ns llx.ai.guardrails-contract-test
  (:require
   [babashka.json :as json]
   [clojure.test :refer [deftest is testing]]
   [llx.ai.impl.adapters.anthropic-messages :as anthropic-messages]
   [llx.ai.impl.adapters.google-generative-ai :as google-generative-ai]
   [llx.ai.impl.client :as client]
   [llx.ai.impl.client.runtime :as runtime]
   [llx.ai.impl.adapters.openai-responses :as openai-responses]
   [llx.ai.impl.registry :as registry]
   [llx.ai.impl.adapters.openai-completions :as openai-completions]
   [llx.ai.impl.utils.unicode :as unicode]))

(set! *warn-on-reflection* true)

(defn- stub-env
  [handler]
  {:http/request             handler
   :json/encode              json/write-str
   :json/decode              (fn [s _opts] (json/read-str s {:key-fn keyword}))
   :json/decode-safe         (fn [s _opts]
                               (try
                                 (json/read-str s {:key-fn keyword})
                                 (catch Exception _ nil)))
   :http/read-body-string    (fn [body] (slurp body))
   :stream/run!              (fn [_] nil)
   :registry                 client/default-registry
   :clock/now-ms             (constantly 1730000000000)
   :id/new                   (constantly "id-1")
   :unicode/sanitize-payload unicode/sanitize-payload})

(def bad-contract-re #"Validation Error|Schema validation failed")

(def test-model
  {:id             "gpt-4o-mini"
   :name           "GPT-4o Mini"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(deftest guardrails-enforces-boundary-shapes
  (let [env (stub-env (fn [_request]
                        {:status 200
                         :body   (json/write-str
                                  {:choices [{:finish_reason "stop"
                                              :message       {:role "assistant" :content "ok"}}]
                                   :usage   {:prompt_tokens 1 :completion_tokens 1 :total_tokens 2}})}))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo bad-contract-re
                          (client/complete env test-model {} {:api-key "x"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo bad-contract-re
                          (client/stream env test-model {} {:api-key "x"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo bad-contract-re
                          (openai-completions/build-request env test-model {} {:api-key "x"} false)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo bad-contract-re
                          (runtime/run-stream! {})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo bad-contract-re
                          (registry/register-adapter (registry/immutable-registry) {:api :openai-completions})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo bad-contract-re
                          (registry/get-adapter (registry/immutable-registry) "openai-responses")))
    (testing "decode-event boundary is enforced for all adapters"
      (doseq [[label decode-event]
              [["openai-completions" openai-completions/decode-event]
               ["openai-responses" openai-responses/decode-event]
               ["anthropic-messages" anthropic-messages/decode-event]
               ["google-generative-ai" google-generative-ai/decode-event]]]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo bad-contract-re
                              (decode-event env {} "{}"))
            label)))))
