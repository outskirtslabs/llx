(ns llx.ai.default-env-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx.ai :as ai]
   [llx.ai.impl.client :as impl.client]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

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

(deftest api-contract-uses-deferred-and-channel-surfaces
  (let [env        {:env :ok}
        model      {:id "m"}
        context    {:messages []}
        complete-d (p/resolved {:role :assistant})
        stream-ch  (sp/chan)]
    (with-redefs [impl.client/complete* (fn [_e _m _c _o] complete-d)
                  impl.client/complete  (fn [_e _m _c _o] complete-d)
                  impl.client/stream*   (fn [_e _m _c _o] stream-ch)
                  impl.client/stream    (fn [_e _m _c _o] stream-ch)]
      (is (p/deferred? (ai/complete* env model context {})))
      (is (p/deferred? (ai/complete env model context {})))
      (is (sp/chan? (ai/stream* env model context {})))
      (is (sp/chan? (ai/stream env model context {}))))))
