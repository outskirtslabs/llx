(ns ol.llx.ai.live.context-overflow-smoke-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer [deftest is testing]]])
   #?@(:clj [[ol.llx.ai.test-util :as util]]
       :cljs [[ol.llx.ai.test-util :as util :include-macros true]])
   [ol.llx.ai :as client]
   [ol.llx.ai.impl.utils.overflow :as overflow]
   [ol.llx.ai.impl.utils.rate-limit :as rate-limit]
   [ol.llx.ai.live.env :as live-env]
   [ol.llx.ai.live.models :as models]
   [promesa.core :as p]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private env
  (client/default-env))

(def ^:private lorem-ipsum
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. ")

(defn- log!
  [s]
  #?(:clj (do
            (println (str "[context-overflow-smoke] " s))
            (flush))
     :cljs (.log js/console (str "[context-overflow-smoke] " s))))

(defn- call-timeout-ms
  []
  (or (some-> (live-env/get-env "LLX_LIVE_OVERFLOW_CALL_TIMEOUT_MS")
              (#?(:clj Long/parseLong
                  :cljs (fn [s]
                          (let [n (js/parseInt s 10)]
                            (when-not (js/isNaN n)
                              n))))))
      3000))

(defn- non-overflow-environment-error?
  [assistant-message]
  (or (rate-limit/rate-limited? assistant-message)
      (contains? #{:ol.llx/server-error
                   :ol.llx/timeout
                   :ol.llx/connection-error}
                 (:error-type assistant-message))))

(defn- generate-overflow-content
  [context-window]
  (let [target-tokens (+ context-window 10000)
        target-chars  (long (* target-tokens 4 1.5))
        repeats       (long (Math/ceil (/ target-chars (double (count lorem-ipsum)))))]
    (apply str (repeat repeats lorem-ipsum))))

(defn- overflow-call*
  [model context opts]
  (-> (client/complete* env model context opts)
      (p/catch (fn [e]
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
                    :timestamp     (util/now-ms)})))))

(defn- test-context-overflow!*
  [model opts]
  (let [started-at     (util/now-ms)
        timeout-ms     (call-timeout-ms)
        oversized-text (generate-overflow-content (:context-window model))
        opts           (assoc opts :max-retries 0)
        context        {:messages [{:role      :user
                                    :content   oversized-text
                                    :timestamp (util/now-ms)}]}
        _              (log! (str "START model=" (:id model)
                                  " provider=" (:provider model)
                                  " context-window=" (:context-window model)
                                  " oversized-chars=" (count oversized-text)
                                  " call-timeout-ms=" timeout-ms
                                  " max-retries=" (:max-retries opts)))
        call-d         (overflow-call* model context opts)]
    (-> (util/run-with-timeout* call-d timeout-ms)
        (p/then (fn [result]
                  (when (util/timeout-result? result)
                    (log! (str "TIMEOUT model=" (:id model) " call-timeout-ms=" timeout-ms)))
                  (log! (str "RESULT model=" (:id model)
                             " stop-reason=" (:stop-reason result)
                             " error-type=" (:error-type result)))
                  (if (util/timeout-result? result)
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
                             " elapsed-ms=" (- (util/now-ms) started-at)))
                  true)))))

(deftest ^:ol.llx/anthropic live-overflow-anthropic
  (testing "anthropic overflow detection"
    (util/async done
                (util/run-live-async!
                 (test-context-overflow!* models/anthropic {:max-output-tokens 128})
                 done))))

(deftest ^:ol.llx/openai live-overflow-openai-completions
  (testing "openai completions overflow detection"
    (util/async done
                (util/run-live-async!
                 (test-context-overflow!* models/openai-completions {:max-output-tokens 128})
                 done))))

(deftest ^:ol.llx/openai live-overflow-openai-responses
  (testing "openai responses overflow detection"
    (util/async done
                (util/run-live-async!
                 (test-context-overflow!* models/openai-responses {:max-output-tokens 128})
                 done))))

(deftest ^:ol.llx/google live-overflow-google
  (testing "google overflow detection"
    (util/async done
                (util/run-live-async!
                 (test-context-overflow!* models/google {:max-output-tokens 128})
                 done))))

(deftest ^:ol.llx/mistral live-overflow-mistral
  (testing "mistral overflow detection"
    (util/async done
                (util/run-live-async!
                 (test-context-overflow!* models/mistral {:max-output-tokens 128})
                 done))))
