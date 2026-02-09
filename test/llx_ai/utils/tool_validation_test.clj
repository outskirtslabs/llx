(ns llx-ai.utils.tool-validation-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.utils.tool-validation :as sut]))

(def tools
  [{:name         "search"
    :description  "Search the web"
    :input-schema [:map {:closed true}
                   [:q :string]
                   [:limit {:optional true} :int]]}])

(deftest validate-tool-call
  (testing "success case"
    (is (= {:q "clojure" :limit 3}
           (sut/validate-tool-call
            tools
            {:name      "search"
             :arguments {:q "clojure" :limit 3}}))))
  (testing "tool not found"
    (let [ex (try
               (sut/validate-tool-call tools {:name "unknown-tool" :arguments {}})
               nil
               (catch clojure.lang.ExceptionInfo e
                 e))]
      (is (some? ex))
      (is (= :tool-not-found (-> ex ex-data :type)))
      (is (= "unknown-tool" (-> ex ex-data :tool-name)))
      (is (= ["search"] (-> ex ex-data :available-tools)))))
  (testing "validation errors are structured"
    (let [ex (try
               (sut/validate-tool-call tools {:name "search" :arguments {:q 123}})
               nil
               (catch clojure.lang.ExceptionInfo e
                 e))]
      (is (some? ex))
      (is (= :validation-error (-> ex ex-data :type)))
      (is (= "search" (-> ex ex-data :tool-name)))
      (is (map? (-> ex ex-data :errors)))
      (is (= {:q 123} (-> ex ex-data :arguments)))))
  (testing "missing required field"
    (let [ex (try
               (sut/validate-tool-call tools {:name "search" :arguments {}})
               nil
               (catch clojure.lang.ExceptionInfo e
                 e))]
      (is (some? ex))
      (is (map? (-> ex ex-data :errors)))
      (is (some? (-> ex ex-data :errors :q))))))
