(ns llx-ai.errors-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.errors :as sut]))

(deftest rate-limit-error-has-correct-type-and-recoverable
  (let [ex (sut/rate-limit "openai" "Rate limit exceeded" :http-status 429 :retry-after 30)]
    (is (= {:type         :llx/rate-limit
            :message      "Rate limit exceeded"
            :recoverable? true
            :provider     "openai"
            :http-status  429
            :retry-after  30}
           (ex-data ex)))
    (is (= "Rate limit exceeded" (ex-message ex)))))

(deftest authentication-error-is-not-recoverable
  (is (= {:type         :llx/authentication-error
          :message      "Invalid API key"
          :recoverable? false
          :provider     "anthropic"
          :http-status  401}
         (ex-data (sut/authentication-error "anthropic" "Invalid API key" :http-status 401)))))

(deftest server-error-is-recoverable
  (is (= {:type         :llx/server-error
          :message      "Internal server error"
          :recoverable? true
          :provider     "google"
          :http-status  500}
         (ex-data (sut/server-error "google" "Internal server error" :http-status 500)))))

(deftest timeout-error-is-recoverable
  (is (= {:type         :llx/timeout
          :message      "Request timeout"
          :recoverable? true
          :provider     "openai"
          :context      {:timeout-ms 30000}}
         (ex-data (sut/timeout-error "openai" "Request timeout" :timeout-ms 30000)))))

(deftest connection-error-is-recoverable
  (is (= {:type         :llx/connection-error
          :message      "Connection refused"
          :recoverable? true
          :provider     "openai"}
         (ex-data (sut/connection-error "openai" "Connection refused")))))

(deftest invalid-request-is-not-recoverable
  (is (= {:type         :llx/invalid-request
          :message      "Bad request"
          :recoverable? false
          :provider     "openai"
          :http-status  400
          :context      {:body "malformed"}}
         (ex-data (sut/invalid-request "Bad request" :provider "openai" :http-status 400
                                       :context {:body "malformed"})))))

(deftest model-not-found-is-not-recoverable
  (is (= {:type         :llx/model-not-found
          :message      "Model not found"
          :recoverable? false
          :provider     "openai"
          :http-status  404}
         (ex-data (sut/model-not-found "openai" "Model not found" :http-status 404)))))

(deftest quota-exceeded-is-not-recoverable
  (is (= {:type         :llx/quota-exceeded
          :message      "Quota exceeded"
          :recoverable? false
          :provider     "openai"
          :http-status  429}
         (ex-data (sut/quota-exceeded "openai" "Quota exceeded" :http-status 429)))))

(deftest content-filter-is-not-recoverable
  (is (= {:type          :llx/content-filter
          :message       "Content filtered"
          :recoverable?  false
          :provider      "anthropic"
          :provider-code "content_policy"}
         (ex-data (sut/content-filter "anthropic" "Content filtered" :provider-code "content_policy")))))

(deftest http-status->error-maps-common-codes
  (testing "400 -> invalid-request"
    (is (= {:type         :llx/invalid-request
            :message      "Bad request"
            :recoverable? false
            :provider     "openai"
            :http-status  400
            :context      {:body nil}}
           (ex-data (sut/http-status->error 400 "openai" "Bad request")))))
  (testing "401 -> authentication-error"
    (is (= {:type         :llx/authentication-error
            :message      "Unauthorized"
            :recoverable? false
            :provider     "openai"
            :http-status  401}
           (ex-data (sut/http-status->error 401 "openai" "Unauthorized")))))
  (testing "403 -> authorization-error"
    (is (= {:type         :llx/authorization-error
            :message      "Forbidden"
            :recoverable? false
            :provider     "openai"
            :http-status  403}
           (ex-data (sut/http-status->error 403 "openai" "Forbidden")))))
  (testing "404 -> model-not-found"
    (is (= {:type         :llx/model-not-found
            :message      "Not found"
            :recoverable? false
            :provider     "openai"
            :http-status  404}
           (ex-data (sut/http-status->error 404 "openai" "Not found")))))
  (testing "408 -> timeout"
    (is (= {:type         :llx/timeout
            :message      "Timeout"
            :recoverable? true
            :provider     "openai"}
           (ex-data (sut/http-status->error 408 "openai" "Timeout")))))
  (testing "500 -> server-error"
    (is (= {:type         :llx/server-error
            :message      "Internal error"
            :recoverable? true
            :provider     "openai"
            :http-status  500}
           (ex-data (sut/http-status->error 500 "openai" "Internal error")))))
  (testing "502 -> server-error"
    (is (= {:type         :llx/server-error
            :message      "Bad gateway"
            :recoverable? true
            :provider     "openai"
            :http-status  502}
           (ex-data (sut/http-status->error 502 "openai" "Bad gateway")))))
  (testing "503 -> server-error"
    (is (= {:type         :llx/server-error
            :message      "Service unavailable"
            :recoverable? true
            :provider     "openai"
            :http-status  503}
           (ex-data (sut/http-status->error 503 "openai" "Service unavailable"))))))

(deftest http-status-429-with-quota-body-returns-quota-exceeded
  (is (= {:type         :llx/quota-exceeded
          :message      "quota exceeded"
          :recoverable? false
          :provider     "openai"
          :http-status  429}
         (ex-data (sut/http-status->error 429 "openai" "quota exceeded"
                                          :body "Your quota has been exceeded")))))

(deftest http-status-429-without-quota-returns-rate-limit
  (is (= {:type         :llx/rate-limit
          :message      "Too many requests"
          :recoverable? true
          :provider     "openai"
          :http-status  429
          :retry-after  10}
         (ex-data (sut/http-status->error 429 "openai" "Too many requests"
                                          :retry-after 10)))))

(deftest http-status-429-with-quota-and-retry-after-returns-rate-limit
  (is (= {:type         :llx/rate-limit
          :message      "quota exceeded, retry later"
          :recoverable? true
          :provider     "google"
          :http-status  429
          :retry-after  14.9}
         (ex-data (sut/http-status->error 429 "google" "quota exceeded, retry later"
                                          :retry-after 14.9
                                          :body "Quota exceeded")))))

(deftest http-status-unknown-5xx-returns-recoverable-provider-error
  (is (= {:type         :llx/provider-error
          :message      "Custom error"
          :recoverable? true
          :provider     "openai"
          :http-status  599}
         (ex-data (sut/http-status->error 599 "openai" "Custom error")))))

(deftest http-status-unknown-4xx-returns-non-recoverable-provider-error
  (is (= {:type         :llx/provider-error
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

(deftest retry-loop-succeeds-on-first-attempt
  (let [result (sut/retry-loop (fn [] :ok) 2 (fn [_ms]))]
    (is (= :ok result))))

(deftest retry-loop-retries-transient-and-succeeds
  (let [call-count (atom 0)
        sleep-ms   (atom [])
        f          (fn []
                     (swap! call-count inc)
                     (if (= 1 @call-count)
                       (throw (sut/rate-limit "openai" "rate limit"))
                       :success))]
    (is (= :success (sut/retry-loop f 2 (fn [ms] (swap! sleep-ms conj ms)))))
    (is (= 2 @call-count))
    (is (= 1 (count @sleep-ms)))))

(deftest retry-loop-exhausts-retries-and-throws
  (let [call-count (atom 0)
        f          (fn []
                     (swap! call-count inc)
                     (throw (sut/rate-limit "openai" "rate limit")))
        ex         (try
                     (sut/retry-loop f 2 (fn [_ms]))
                     (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:type         :llx/rate-limit
            :message      "rate limit"
            :recoverable? true
            :provider     "openai"
            :http-status  429}
           (ex-data ex)))
    ;; 1 initial + 2 retries = 3 total
    (is (= 3 @call-count))))

(deftest retry-loop-does-not-retry-client-errors
  (let [call-count (atom 0)
        f          (fn []
                     (swap! call-count inc)
                     (throw (sut/authentication-error "openai" "bad key")))
        ex         (try
                     (sut/retry-loop f 2 (fn [_ms]))
                     (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:type         :llx/authentication-error
            :message      "bad key"
            :recoverable? false
            :provider     "openai"
            :http-status  401}
           (ex-data ex)))
    (is (= 1 @call-count))))

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
  (is (= {:type         :llx/provider-error
          :message      "weird error"
          :recoverable? true
          :provider     "openai"
          :http-status  599}
         (ex-data (sut/provider-error "openai" "weird error"
                                      :http-status 599 :recoverable? true))))
  (is (= {:type         :llx/provider-error
          :message      "weird error"
          :recoverable? false
          :provider     "openai"
          :http-status  450}
         (ex-data (sut/provider-error "openai" "weird error"
                                      :http-status 450 :recoverable? false)))))
