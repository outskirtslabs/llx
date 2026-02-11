(ns llx.ai.default-env-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx.ai :as ai]
   [promesa.core :as p]))

(deftest default-env-returns-cljs-node-scaffold
  (let [env (ai/default-env)]
    (testing "required env hooks are present"
      (is (fn? (:http/request env)))
      (is (fn? (:json/encode env)))
      (is (fn? (:json/decode env)))
      (is (fn? (:stream/run! env)))
      (is (fn? (:clock/now-ms env)))
      (is (fn? (:id/new env)))
      (is (fn? (:unicode/sanitize-payload env))))
    (testing "json/decode-safe returns nil for invalid JSON"
      (is (nil? ((:json/decode-safe env) "{" {}))))
    (testing "node scaffold exposes explicit not-implemented failures"
      (let [ex (try
                 ((:http/request env) {:url "https://example.invalid"})
                 (catch :default e
                   e))]
        (is (= :llx/not-implemented (:type (ex-data ex))))
        (is (= :http/request (:feature (ex-data ex))))))
    (testing "promesa is available in cljs test runtime"
      (is (= 42 @(p/resolved 42))))))
