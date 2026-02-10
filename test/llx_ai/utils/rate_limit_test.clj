(ns llx-ai.utils.rate-limit-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.utils.rate-limit :as sut]))

(deftest rate-limited-detection
  (testing "detects structured error types"
    (is (true? (sut/rate-limited? {:stop-reason :error :error-type :llx/rate-limit})))
    (is (true? (sut/rate-limited? {:stop-reason :error :error-type :llx/quota-exceeded}))))
  (testing "detects provider message patterns"
    (is (true? (sut/rate-limited? {:stop-reason   :error
                                   :error-message "429 Too Many Requests"})))
    (is (true? (sut/rate-limited? {:stop-reason   :error
                                   :error-message "You exceeded your current quota. Please retry in 14.2s."}))))
  (testing "false for non-rate-limit errors"
    (is (false? (sut/rate-limited? {:stop-reason   :error
                                    :error-message "invalid api key"})))
    (is (false? (sut/rate-limited? {:stop-reason :stop
                                    :content     [{:type :text :text "ok"}]})))))
