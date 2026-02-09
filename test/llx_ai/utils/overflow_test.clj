(ns llx-ai.utils.overflow-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.utils.overflow :as sut]))

(deftest context-overflow-detects-explicit-provider-messages
  (testing "matches common provider overflow phrasing"
    (is (true? (sut/context-overflow?
                {:stop-reason   :error
                 :error-message "prompt is too long: 213462 tokens > 200000 maximum"})))
    (is (true? (sut/context-overflow?
                {:stop-reason   :error
                 :error-message "Your input exceeds the context window of this model"})))
    (is (true? (sut/context-overflow?
                {:stop-reason   :error
                 :error-message "The input token count (1196265) exceeds the maximum number of tokens allowed"})))))

(deftest context-overflow-detects-400-413-no-body-signature
  (is (true? (sut/context-overflow?
              {:stop-reason   :error
               :error-message "400 status code (no body)"})))
  (is (true? (sut/context-overflow?
              {:stop-reason   :error
               :error-message "413 (no body)"}))))

(deftest context-overflow-detects-silent-overflow-with-context-window
  (is (true? (sut/context-overflow?
              {:stop-reason :stop
               :usage       {:input 1200 :cache-read 50}}
              1024)))
  (is (false? (sut/context-overflow?
               {:stop-reason :stop
                :usage       {:input 700 :cache-read 200}}
               1024))))

(deftest context-overflow-false-for-non-overflow-errors
  (is (false? (sut/context-overflow?
               {:stop-reason   :error
                :error-message "429 status code (rate limited)"})))
  (is (false? (sut/context-overflow?
               {:stop-reason   :error
                :error-message "invalid api key"}))))
