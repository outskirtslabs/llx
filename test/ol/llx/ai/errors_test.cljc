(ns ol.llx.ai.errors-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [async deftest is testing]]])
   [ol.llx.ai.impl.errors :as sut]
   [ol.llx.ai.test-util :as util]
   [promesa.core :as p]))

#?(:cljs
   (defn- fail-and-done!
     [done err]
     (is nil (str err))
     (done)))

(deftest rate-limit-error-has-correct-type-and-recoverable
  (let [ex (sut/rate-limit "openai" "Rate limit exceeded" :http-status 429 :retry-after 30)]
    (is (= {:type         :ol.llx/rate-limit
            :message      "Rate limit exceeded"
            :recoverable? true
            :provider     "openai"
            :http-status  429
            :retry-after  30}
           (ex-data ex)))
    (is (= "Rate limit exceeded" (ex-message ex)))))

(deftest authentication-error-is-not-recoverable
  (is (= {:type         :ol.llx/authentication-error
          :message      "Invalid API key"
          :recoverable? false
          :provider     "anthropic"
          :http-status  401}
         (ex-data (sut/authentication-error "anthropic" "Invalid API key" :http-status 401)))))

(deftest server-error-is-recoverable
  (is (= {:type         :ol.llx/server-error
          :message      "Internal server error"
          :recoverable? true
          :provider     "google"
          :http-status  500}
         (ex-data (sut/server-error "google" "Internal server error" :http-status 500)))))

(deftest timeout-error-is-recoverable
  (is (= {:type         :ol.llx/timeout
          :message      "Request timeout"
          :recoverable? true
          :provider     "openai"
          :context      {:timeout-ms 30000}}
         (ex-data (sut/timeout-error "openai" "Request timeout" :timeout-ms 30000)))))

(deftest connection-error-is-recoverable
  (is (= {:type         :ol.llx/connection-error
          :message      "Connection refused"
          :recoverable? true
          :provider     "openai"}
         (ex-data (sut/connection-error "openai" "Connection refused")))))

(deftest invalid-request-is-not-recoverable
  (is (= {:type         :ol.llx/invalid-request
          :message      "Bad request"
          :recoverable? false
          :provider     "openai"
          :http-status  400
          :context      {:body "malformed"}}
         (ex-data (sut/invalid-request "Bad request" :provider "openai" :http-status 400
                                       :context {:body "malformed"})))))

(deftest model-not-found-is-not-recoverable
  (is (= {:type         :ol.llx/model-not-found
          :message      "Model not found"
          :recoverable? false
          :provider     "openai"
          :http-status  404}
         (ex-data (sut/model-not-found "openai" "Model not found" :http-status 404)))))

(deftest quota-exceeded-is-not-recoverable
  (is (= {:type         :ol.llx/quota-exceeded
          :message      "Quota exceeded"
          :recoverable? false
          :provider     "openai"
          :http-status  429}
         (ex-data (sut/quota-exceeded "openai" "Quota exceeded" :http-status 429)))))

(deftest retry-delay-exceeded-is-not-recoverable
  (is (= {:type         :ol.llx/retry-delay-exceeded
          :message      "Server requested 120.0s retry delay (max: 60.0s)."
          :recoverable? false
          :provider     "google"
          :request-id   "req_1"
          :context      {:requested-delay-ms 120000
                         :max-delay-ms       60000}}
         (ex-data (sut/retry-delay-exceeded "google" 120000 60000 :request-id "req_1")))))

(deftest content-filter-is-not-recoverable
  (is (= {:type          :ol.llx/content-filter
          :message       "Content filtered"
          :recoverable?  false
          :provider      "anthropic"
          :provider-code "content_policy"}
         (ex-data (sut/content-filter "anthropic" "Content filtered" :provider-code "content_policy")))))

(deftest http-status->error-maps-common-codes
  (testing "400 -> invalid-request"
    (is (= {:type         :ol.llx/invalid-request
            :message      "Bad request"
            :recoverable? false
            :provider     "openai"
            :http-status  400
            :context      {:body nil}}
           (ex-data (sut/http-status->error 400 "openai" "Bad request")))))
  (testing "401 -> authentication-error"
    (is (= {:type         :ol.llx/authentication-error
            :message      "Unauthorized"
            :recoverable? false
            :provider     "openai"
            :http-status  401}
           (ex-data (sut/http-status->error 401 "openai" "Unauthorized")))))
  (testing "403 -> authorization-error"
    (is (= {:type         :ol.llx/authorization-error
            :message      "Forbidden"
            :recoverable? false
            :provider     "openai"
            :http-status  403}
           (ex-data (sut/http-status->error 403 "openai" "Forbidden")))))
  (testing "404 -> model-not-found"
    (is (= {:type         :ol.llx/model-not-found
            :message      "Not found"
            :recoverable? false
            :provider     "openai"
            :http-status  404}
           (ex-data (sut/http-status->error 404 "openai" "Not found")))))
  (testing "408 -> timeout"
    (is (= {:type         :ol.llx/timeout
            :message      "Timeout"
            :recoverable? true
            :provider     "openai"}
           (ex-data (sut/http-status->error 408 "openai" "Timeout")))))
  (testing "500 -> server-error"
    (is (= {:type         :ol.llx/server-error
            :message      "Internal error"
            :recoverable? true
            :provider     "openai"
            :http-status  500}
           (ex-data (sut/http-status->error 500 "openai" "Internal error")))))
  (testing "502 -> server-error"
    (is (= {:type         :ol.llx/server-error
            :message      "Bad gateway"
            :recoverable? true
            :provider     "openai"
            :http-status  502}
           (ex-data (sut/http-status->error 502 "openai" "Bad gateway")))))
  (testing "503 -> server-error"
    (is (= {:type         :ol.llx/server-error
            :message      "Service unavailable"
            :recoverable? true
            :provider     "openai"
            :http-status  503}
           (ex-data (sut/http-status->error 503 "openai" "Service unavailable"))))))

(deftest http-status-429-with-quota-body-returns-quota-exceeded
  (is (= {:type         :ol.llx/quota-exceeded
          :message      "quota exceeded"
          :recoverable? false
          :provider     "openai"
          :http-status  429}
         (ex-data (sut/http-status->error 429 "openai" "quota exceeded"
                                          :body "Your quota has been exceeded")))))

(deftest http-status-429-without-quota-returns-rate-limit
  (is (= {:type         :ol.llx/rate-limit
          :message      "Too many requests"
          :recoverable? true
          :provider     "openai"
          :http-status  429
          :retry-after  10}
         (ex-data (sut/http-status->error 429 "openai" "Too many requests"
                                          :retry-after 10)))))

(deftest http-status-429-with-quota-and-retry-after-returns-rate-limit
  (is (= {:type         :ol.llx/rate-limit
          :message      "quota exceeded, retry later"
          :recoverable? true
          :provider     "google"
          :http-status  429
          :retry-after  14.9}
         (ex-data (sut/http-status->error 429 "google" "quota exceeded, retry later"
                                          :retry-after 14.9
                                          :body "Quota exceeded")))))

(deftest http-status-unknown-5xx-returns-recoverable-provider-error
  (is (= {:type         :ol.llx/provider-error
          :message      "Custom error"
          :recoverable? true
          :provider     "openai"
          :http-status  599}
         (ex-data (sut/http-status->error 599 "openai" "Custom error")))))

(deftest http-status-unknown-4xx-returns-non-recoverable-provider-error
  (is (= {:type         :ol.llx/provider-error
          :message      "I'm a teapot"
          :recoverable? false
          :provider     "openai"
          :http-status  418}
         (ex-data (sut/http-status->error 418 "openai" "I'm a teapot")))))

(deftest predicates-match-error-categories
  (testing "llx-error? matches structured errors"
    (is (sut/llx-error? (sut/rate-limit "openai" "rate limit")))
    (is (not (sut/llx-error? (ex-info "plain error" {})))))

  (testing "recoverable? matches recoverable errors"
    (is (sut/recoverable? (sut/rate-limit "openai" "rate limit")))
    (is (sut/recoverable? (sut/server-error "openai" "server error")))
    (is (not (sut/recoverable? (sut/authentication-error "openai" "bad key")))))

  (testing "rate-limit-error?"
    (is (sut/rate-limit-error? (sut/rate-limit "openai" "rate limit")))
    (is (not (sut/rate-limit-error? (sut/server-error "openai" "server error")))))

  (testing "quota-exceeded-error?"
    (is (sut/quota-exceeded-error? (sut/quota-exceeded "openai" "quota exceeded")))
    (is (not (sut/quota-exceeded-error? (sut/rate-limit "openai" "rate limit")))))

  (testing "rate-limited-error?"
    (is (sut/rate-limited-error? (sut/rate-limit "openai" "rate limit")))
    (is (sut/rate-limited-error? (sut/quota-exceeded "openai" "quota exceeded")))
    (is (not (sut/rate-limited-error? (sut/server-error "openai" "server error")))))

  (testing "timeout-error?"
    (is (sut/timeout-error? (sut/timeout-error "openai" "timeout")))
    (is (not (sut/timeout-error? (sut/rate-limit "openai" "rate limit")))))

  (testing "client-error?"
    (is (sut/client-error? (sut/authentication-error "openai" "bad key")))
    (is (sut/client-error? (sut/authorization-error "openai" "forbidden")))
    (is (sut/client-error? (sut/invalid-request "bad")))
    (is (not (sut/client-error? (sut/rate-limit "openai" "rate limit")))))

  (testing "transient-error?"
    (is (sut/transient-error? (sut/rate-limit "openai" "rate limit")))
    (is (sut/transient-error? (sut/server-error "openai" "server error")))
    (is (sut/transient-error? (sut/timeout-error "openai" "timeout")))
    (is (sut/transient-error? (sut/connection-error "openai" "conn refused")))
    (is (not (sut/transient-error? (sut/authentication-error "openai" "bad key"))))))

(deftest should-retry-allows-transient-errors
  (is (sut/should-retry? (sut/rate-limit "openai" "rate limit") :max-retries 2 :current-retry 0))
  (is (sut/should-retry? (sut/server-error "openai" "server error") :max-retries 2 :current-retry 0))
  (is (sut/should-retry? (sut/timeout-error "openai" "timeout") :max-retries 2 :current-retry 0))
  (is (sut/should-retry? (sut/connection-error "openai" "conn refused") :max-retries 2 :current-retry 0)))

(deftest should-retry-rejects-client-errors
  (is (not (sut/should-retry? (sut/authentication-error "openai" "bad key") :max-retries 2 :current-retry 0)))
  (is (not (sut/should-retry? (sut/authorization-error "openai" "forbidden") :max-retries 2 :current-retry 0)))
  (is (not (sut/should-retry? (sut/invalid-request "bad request") :max-retries 2 :current-retry 0))))

(deftest should-retry-respects-max-retries
  (is (not (sut/should-retry? (sut/rate-limit "openai" "rate limit") :max-retries 2 :current-retry 2)))
  (is (not (sut/should-retry? (sut/rate-limit "openai" "rate limit") :max-retries 2 :current-retry 3)))
  (is (sut/should-retry? (sut/rate-limit "openai" "rate limit") :max-retries 2 :current-retry 1)))

(deftest retry-delay-uses-server-retry-after-for-rate-limit
  (let [ex (sut/rate-limit "openai" "rate limit" :retry-after 30)]
    (is (= 30000 (sut/retry-delay-ms ex 0)))))

(deftest retry-delay-uses-exponential-backoff-for-rate-limit-without-header
  (let [ex (sut/rate-limit "openai" "rate limit")]
    (is (= 1000 (sut/retry-delay-ms ex 0)))
    (is (= 2000 (sut/retry-delay-ms ex 1)))
    (is (= 4000 (sut/retry-delay-ms ex 2)))))

(deftest retry-delay-adds-jitter-for-server-errors
  ;; retry-count 0: base is 1000, jitter 0..999 -> range [1000, 1999]
  (let [ex     (sut/server-error "openai" "server error")
        delays (repeatedly 20 #(sut/retry-delay-ms ex 0))]
    (is (every? #(and (>= % 1000) (< % 2000)) delays))))

(deftest retry-delay-uses-linear-backoff-for-connection-errors
  (let [ex (sut/connection-error "openai" "conn refused")]
    (is (= 1000 (sut/retry-delay-ms ex 0)))
    (is (= 2000 (sut/retry-delay-ms ex 1)))
    (is (= 3000 (sut/retry-delay-ms ex 2)))))

(deftest retry-loop-async-succeeds-on-first-attempt
  #?(:clj
     (let [result (sut/retry-loop-async (fn [] :ok) 2 (fn [_ms]))]
       (is (p/deferred? result))
       (is (= :ok (util/await! result))))
     :cljs
     (async done
            (let [result (sut/retry-loop-async (fn [] :ok) 2 (fn [_ms]))]
              (is (p/deferred? result))
              (-> result
                  (p/then (fn [value]
                            (is (= :ok value))
                            (done)))
                  (p/catch (partial fail-and-done! done)))))))

(deftest retry-loop-async-retries-transient-and-succeeds
  #?(:clj
     (let [call-count (atom 0)
           sleep-ms   (atom [])
           f          (fn []
                        (swap! call-count inc)
                        (if (= 1 @call-count)
                          (throw (sut/rate-limit "openai" "rate limit"))
                          :success))]
       (is (= :success (util/await! (sut/retry-loop-async f 2 (fn [ms] (swap! sleep-ms conj ms))))))
       (is (= 2 @call-count))
       (is (= 1 (count @sleep-ms))))
     :cljs
     (async done
            (let [call-count (atom 0)
                  sleep-ms   (atom [])
                  f          (fn []
                               (swap! call-count inc)
                               (if (= 1 @call-count)
                                 (throw (sut/rate-limit "openai" "rate limit"))
                                 :success))]
              (-> (sut/retry-loop-async f 2 (fn [ms] (swap! sleep-ms conj ms)))
                  (p/then (fn [value]
                            (is (= :success value))
                            (is (= 2 @call-count))
                            (is (= 1 (count @sleep-ms)))
                            (done)))
                  (p/catch (partial fail-and-done! done)))))))

(deftest retry-loop-async-fails-when-server-retry-delay-exceeds-cap
  #?(:clj
     (let [f  (fn []
                (throw (sut/rate-limit "google" "rate limit"
                                       :retry-after 10
                                       :request-id "req_1")))
           ex (try
                (util/await! (sut/retry-loop-async f 2 (fn [_ms])
                                                   {:provider           "google"
                                                    :max-retry-delay-ms 5000}))
                nil
                (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                  e))]
       (is (= {:type         :ol.llx/retry-delay-exceeded
               :message      "Server requested 10.0s retry delay (max: 5.0s)."
               :recoverable? false
               :provider     "google"
               :request-id   "req_1"
               :context      {:requested-delay-ms 10000
                              :max-delay-ms       5000}}
              (ex-data ex))))
     :cljs
     (async done
            (let [f (fn []
                      (throw (sut/rate-limit "google" "rate limit"
                                             :retry-after 10
                                             :request-id "req_1")))]
              (-> (sut/retry-loop-async f 2 (fn [_ms])
                                        {:provider           "google"
                                         :max-retry-delay-ms 5000})
                  (p/then (fn [_]
                            (is nil "expected rejection")
                            (done)))
                  (p/catch (fn [ex]
                             (is (= {:type         :ol.llx/retry-delay-exceeded
                                     :message      "Server requested 10.0s retry delay (max: 5.0s)."
                                     :recoverable? false
                                     :provider     "google"
                                     :request-id   "req_1"
                                     :context      {:requested-delay-ms 10000
                                                    :max-delay-ms       5000}}
                                    (ex-data ex)))
                             (done))))))))

(deftest retry-loop-async-emits-retry-scheduled-trove-signal
  #?(:clj
     (util/with-captured-logs!
       (fn [logs*]
         (let [call-count (atom 0)
               f          (fn []
                            (swap! call-count inc)
                            (if (= 1 @call-count)
                              (throw (sut/rate-limit "openai" "rate limit"
                                                     :retry-after 2
                                                     :request-id "req_1"))
                              :success))
               _          (util/await! (sut/retry-loop-async f 2 (fn [_ms]) {:call-id "call_1" :provider "openai"}))
               event      (util/first-event logs* :ol.llx.obs/retry-scheduled)]
           (is (util/submap?
                {:id    :ol.llx.obs/retry-scheduled
                 :level :info
                 :data  {:call-id      "call_1"
                         :attempt      0
                         :next-attempt 1
                         :max-retries  2
                         :delay-ms     2000
                         :error-type   :ol.llx/rate-limit
                         :provider     "openai"
                         :request-id   "req_1"
                         :retry-after  2}}
                (util/strip-generated event))))))
     :cljs
     (async done
            (util/with-captured-logs!
              (fn [logs*]
                (let [call-count (atom 0)
                      f          (fn []
                                   (swap! call-count inc)
                                   (if (= 1 @call-count)
                                     (throw (sut/rate-limit "openai" "rate limit"
                                                            :retry-after 2
                                                            :request-id "req_1"))
                                     :success))]
                  (-> (sut/retry-loop-async f 2 (fn [_ms]) {:call-id "call_1" :provider "openai"})
                      (p/then (fn [_]
                                (let [event (util/first-event logs* :ol.llx.obs/retry-scheduled)]
                                  (is (util/submap?
                                       {:id    :ol.llx.obs/retry-scheduled
                                        :level :info
                                        :data  {:call-id      "call_1"
                                                :attempt      0
                                                :next-attempt 1
                                                :max-retries  2
                                                :delay-ms     2000
                                                :error-type   :ol.llx/rate-limit
                                                :provider     "openai"
                                                :request-id   "req_1"
                                                :retry-after  2}}
                                       (util/strip-generated event))))
                                (done)))
                      (p/catch (partial fail-and-done! done)))))))))

(deftest retry-loop-async-exhausts-retries-and-throws
  #?(:clj
     (let [call-count (atom 0)
           f          (fn []
                        (swap! call-count inc)
                        (throw (sut/rate-limit "openai" "rate limit")))
           ex         (try
                        (util/await! (sut/retry-loop-async f 2 (fn [_ms])))
                        (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e e))]
       (is (= {:type         :ol.llx/rate-limit
               :message      "rate limit"
               :recoverable? true
               :provider     "openai"
               :http-status  429}
              (ex-data ex)))
       ;; 1 initial + 2 retries = 3 total
       (is (= 3 @call-count)))
     :cljs
     (async done
            (let [call-count (atom 0)
                  f          (fn []
                               (swap! call-count inc)
                               (throw (sut/rate-limit "openai" "rate limit")))]
              (-> (sut/retry-loop-async f 2 (fn [_ms]))
                  (p/then (fn [value]
                            (is (nil? value))
                            (done)))
                  (p/catch (fn [e]
                             (is (= {:type         :ol.llx/rate-limit
                                     :message      "rate limit"
                                     :recoverable? true
                                     :provider     "openai"
                                     :http-status  429}
                                    (ex-data e)))
                             (is (= 3 @call-count))
                             (done))))))))

(deftest retry-loop-async-does-not-retry-client-errors
  #?(:clj
     (let [call-count (atom 0)
           f          (fn []
                        (swap! call-count inc)
                        (throw (sut/authentication-error "openai" "bad key")))
           ex         (try
                        (util/await! (sut/retry-loop-async f 2 (fn [_ms])))
                        (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e e))]
       (is (= {:type         :ol.llx/authentication-error
               :message      "bad key"
               :recoverable? false
               :provider     "openai"
               :http-status  401}
              (ex-data ex)))
       (is (= 1 @call-count)))
     :cljs
     (async done
            (let [call-count (atom 0)
                  f          (fn []
                               (swap! call-count inc)
                               (throw (sut/authentication-error "openai" "bad key")))]
              (-> (sut/retry-loop-async f 2 (fn [_ms]))
                  (p/then (fn [value]
                            (is (nil? value))
                            (done)))
                  (p/catch (fn [e]
                             (is (= {:type         :ol.llx/authentication-error
                                     :message      "bad key"
                                     :recoverable? false
                                     :provider     "openai"
                                     :http-status  401}
                                    (ex-data e)))
                             (is (= 1 @call-count))
                             (done))))))))

(deftest extracts-retry-after-seconds-header
  (is (= 30.0 (sut/extract-retry-after {"retry-after" "30"}))))

(deftest extracts-retry-after-ms-header
  (is (= 5.0 (sut/extract-retry-after {"retry-after-ms" "5000"}))))

(deftest extracts-x-ratelimit-reset-after-header
  (is (= 10.0 (sut/extract-retry-after {"x-ratelimit-reset-after" "10"}))))

(deftest caps-at-max-seconds
  (is (= 60.0 (sut/extract-retry-after {"retry-after" "120"})))
  (is (= 30.0 (sut/extract-retry-after {"retry-after" "120"} :max-seconds 30))))

(deftest returns-nil-when-no-header
  (is (nil? (sut/extract-retry-after {})))
  (is (nil? (sut/extract-retry-after nil))))

(deftest retry-after-ms-takes-priority
  (is (= 5.0 (sut/extract-retry-after {"retry-after-ms" "5000" "retry-after" "30"}))))

(deftest extract-retry-after-from-message-parses-seconds
  (is (= 14.927068271
         (sut/extract-retry-after-from-message
          "Please retry in 14.927068271s."))))

(deftest extract-retry-after-from-message-returns-nil-when-missing
  (is (nil? (sut/extract-retry-after-from-message "Quota exceeded"))))

(deftest provider-error-with-custom-recoverability
  (is (= {:type         :ol.llx/provider-error
          :message      "weird error"
          :recoverable? true
          :provider     "openai"
          :http-status  599}
         (ex-data (sut/provider-error "openai" "weird error"
                                      :http-status 599 :recoverable? true))))
  (is (= {:type         :ol.llx/provider-error
          :message      "weird error"
          :recoverable? false
          :provider     "openai"
          :http-status  450}
         (ex-data (sut/provider-error "openai" "weird error"
                                      :http-status 450 :recoverable? false)))))
