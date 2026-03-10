(ns llx.ai.adapters.openai-codex-responses-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [llx.ai.impl.adapters.openai-codex-responses :as sut]
   [llx.ai.impl.utils.unicode :as unicode]
   [llx.ai.test-util :as util]))

#?(:clj (set! *warn-on-reflection* true))

(def account-token
  "aaa.eyJodHRwczovL2FwaS5vcGVuYWkuY29tL2F1dGgiOnsiY2hhdGdwdF9hY2NvdW50X2lkIjoiYWNjX3Rlc3QifX0.bbb")

(def codex-model
  {:id             "gpt-5.2-codex"
   :name           "GPT-5.2 Codex"
   :provider       :openai-codex
   :api            :openai-codex-responses
   :base-url       "https://chatgpt.com/backend-api"
   :context-window 400000
   :max-tokens     128000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(defn- stub-env
  ([]
   (stub-env {}))
  ([overrides]
   (merge
    {:http/request             (fn [_] {:status 200 :body "{}"})
     :json/encode              util/json-write
     :json/decode              (fn [s _opts] (util/json-read s {:key-fn keyword}))
     :json/decode-safe         (fn [s _opts]
                                 (util/json-read-safe s {:key-fn keyword}))
     :clock/now-ms             (fn [] 1730000000000)
     :id/new                   (fn [] "id-1")
     :unicode/sanitize-payload unicode/sanitize-payload
     :env/get                  (fn [_k] nil)}
    overrides)))

(deftest build-request-shapes-codex-url-and-headers
  (let [context {:system-prompt "You are helpful."
                 :messages      [{:role :user :content "hello" :timestamp 1}]}
        request (sut/build-request (stub-env)
                                   codex-model
                                   context
                                   {:api-key       account-token
                                    :session-id    "session-1"
                                    :cache-control :short}
                                   true)
        payload (util/json-read (:body request) {:key-fn keyword})]
    (is (= "https://chatgpt.com/backend-api/codex/responses" (:url request)))
    (is (= "You are helpful." (:instructions payload)))
    (is (= (str "Bearer " account-token)
           (get-in request [:headers "Authorization"])))
    (is (= "acc_test" (get-in request [:headers "chatgpt-account-id"])))
    (is (= "responses=experimental" (get-in request [:headers "OpenAI-Beta"])))
    (is (= "session-1" (get-in request [:headers "session_id"])))
    (is (= "session-1" (:prompt_cache_key payload)))))

(deftest build-request-prefers-explicit-api-key-over-env
  (let [context {:messages [{:role :user :content "hello" :timestamp 1}]}
        request (sut/build-request
                 (stub-env {:env/get (fn [k]
                                       (when (= "OPENAI_API_KEY" k)
                                         "env-token"))})
                 codex-model
                 context
                 {:api-key account-token}
                 false)]
    (is (= (str "Bearer " account-token)
           (get-in request [:headers "Authorization"])))))

(deftest build-request-strips-codex-unsupported-payload-fields
  (let [context {:system-prompt "You are helpful."
                 :messages      [{:role :user :content "hello" :timestamp 1}]}
        request (sut/build-request
                 (stub-env)
                 codex-model
                 context
                 {:api-key           account-token
                  :session-id        "session-1"
                  :max-output-tokens 256}
                 false)
        payload (util/json-read (:body request) {:key-fn keyword})]
    (is (nil? (:max_output_tokens payload)))
    (is (nil? (:prompt_cache_retention payload)))))

(deftest build-request-rejects-token-missing-account-id
  (let [context {:messages [{:role :user :content "hello" :timestamp 1}]}]
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         #"account"
         (sut/build-request
          (stub-env)
          codex-model
          context
          {:api-key "aaa.invalid.bbb"}
          false)))))
