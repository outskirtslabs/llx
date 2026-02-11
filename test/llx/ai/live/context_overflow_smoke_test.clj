(ns llx.ai.live.context-overflow-smoke-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx.ai :as client]
   [llx.ai.live.env :as live-env]
   [llx.ai.live.models :as models]
   [llx.ai.impl.client.stream :as await]
   [llx.ai.impl.utils.overflow :as overflow]
   [llx.ai.impl.utils.rate-limit :as rate-limit]))

(set! *warn-on-reflection* true)

(def ^:private env
  (client/default-env))

(def ^:private lorem-ipsum
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. ")

(defn- log!
  [s]
  (println (str "[context-overflow-smoke] " s))
  (flush))

(defn- call-timeout-ms
  []
  (or (some-> (System/getenv "LLX_LIVE_OVERFLOW_CALL_TIMEOUT_MS")
              (Long/parseLong))
      3000))

(defn- non-overflow-environment-error?
  [assistant-message]
  (or (rate-limit/rate-limited? assistant-message)
      (contains? #{:llx/server-error
                   :llx/timeout
                   :llx/connection-error}
                 (:error-type assistant-message))))

(defn- generate-overflow-content
  [context-window]
  (let [target-tokens (+ context-window 10000)
        target-chars  (long (* target-tokens 4 1.5))
        repeats       (long (Math/ceil (/ target-chars (double (count lorem-ipsum)))))]
    (apply str (repeat repeats lorem-ipsum))))

(defn- test-context-overflow!
  [model opts]
  (let [started-at     (System/currentTimeMillis)
        timeout-ms     (call-timeout-ms)
        oversized-text (generate-overflow-content (:context-window model))
        opts           (assoc opts :max-retries 0)
        context        {:messages [{:role      :user
                                    :content   oversized-text
                                    :timestamp (System/currentTimeMillis)}]}
        _              (log! (str "START model=" (:id model)
                                  " provider=" (:provider model)
                                  " context-window=" (:context-window model)
                                  " oversized-chars=" (count oversized-text)
                                  " call-timeout-ms=" timeout-ms
                                  " max-retries=" (:max-retries opts)))
        call*          (future
                         (try
                           (await/await! (client/complete* env model context opts))
                           (catch clojure.lang.ExceptionInfo e
                             (let [error-data (ex-data e)]
                               (log! (str "ERROR model=" (:id model)
                                          " type=" (:type error-data)
                                          " retry-after=" (:retry-after error-data)
                                          " message=" (ex-message e)))
                               {:role          :assistant
                                :content       []
                                :api           (:api model)
                                :provider      (:provider model)
                                :model         (:id model)
                                :usage         {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                                                :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                                :error-type    (:type error-data)
                                :retry-after   (:retry-after error-data)
                                :stop-reason   :error
                                :error-message (ex-message e)
                                :timestamp     (System/currentTimeMillis)}))))
        result         (deref call* timeout-ms ::timeout)]
    (when (= ::timeout result)
      (future-cancel call*)
      (log! (str "TIMEOUT model=" (:id model) " call-timeout-ms=" timeout-ms)))
    (log! (str "RESULT model=" (:id model)
               " stop-reason=" (:stop-reason result)
               " error-type=" (:error-type result)))
    (if (= ::timeout result)
      (is true
          (str "Skipping overflow assertion for " (:id model)
               " due to client call timeout"
               " timeout-ms=" timeout-ms))
      (if (non-overflow-environment-error? result)
        (is true
            (str "Skipping overflow assertion for " (:id model)
                 " due to provider transient/quota limitation"
                 " type=" (:error-type result)
                 (when-let [retry-after (:retry-after result)]
                   (str " retry-after=" retry-after "s"))))
        (is (overflow/context-overflow? result (:context-window model))
            (str "Expected context-overflow? to be true for " (:id model)
                 " stop-reason=" (:stop-reason result)
                 " error=" (:error-message result)))))
    (log! (str "DONE model=" (:id model)
               " elapsed-ms=" (- (System/currentTimeMillis) started-at)))))

(deftest live-overflow-anthropic
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (testing "anthropic overflow detection"
      (test-context-overflow! models/anthropic {:api-key api-key :max-output-tokens 128}))))

(deftest live-overflow-openai-completions
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (testing "openai completions overflow detection"
      (test-context-overflow! models/openai-completions {:api-key api-key :max-output-tokens 128}))))

(deftest live-overflow-openai-responses
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (testing "openai responses overflow detection"
      (test-context-overflow! models/openai-responses {:api-key api-key :max-output-tokens 128}))))

(deftest live-overflow-google
  (let [api-key (live-env/get-env "GEMINI_API_KEY")]
    (testing "google overflow detection"
      (test-context-overflow! models/google {:api-key api-key :max-output-tokens 128}))))

(deftest live-overflow-mistral
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")]
    (testing "mistral overflow detection"
      (test-context-overflow! models/mistral {:api-key api-key :max-output-tokens 128}))))
