(ns llx.ai.utils.overflow-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [llx.ai.impl.utils.overflow :as sut]))

(deftest context-overflow-detection
  (testing "detects explicit provider messages"
    (is (true? (sut/context-overflow?
                {:stop-reason   :error
                 :error-message "prompt is too long: 213462 tokens > 200000 maximum"})))
    (is (true? (sut/context-overflow?
                {:stop-reason   :error
                 :error-message "Your input exceeds the context window of this model"})))
    (is (true? (sut/context-overflow?
                {:stop-reason   :error
                 :error-message "The input token count (1196265) exceeds the maximum number of tokens allowed"}))))
  (testing "detects 400/413 status codes"
    (is (true? (sut/context-overflow?
                {:stop-reason   :error
                 :error-message "400 status code (no body)"})))
    (is (true? (sut/context-overflow?
                {:stop-reason   :error
                 :error-message "413 (no body)"}))))
  (testing "detects silent overflow with context window"
    (is (true? (sut/context-overflow?
                {:stop-reason :stop
                 :usage       {:input 1200 :cache-read 50}}
                1024)))
    (is (false? (sut/context-overflow?
                 {:stop-reason :stop
                  :usage       {:input 700 :cache-read 200}}
                 1024))))
  (testing "false for non-overflow errors"
    (is (false? (sut/context-overflow?
                 {:stop-reason   :error
                  :error-message "429 status code (rate limited)"})))
    (is (false? (sut/context-overflow?
                 {:stop-reason   :error
                  :error-message "invalid api key"})))))
