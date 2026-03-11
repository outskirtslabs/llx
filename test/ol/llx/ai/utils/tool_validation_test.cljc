(ns ol.llx.ai.utils.tool-validation-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [clojure.string :as str]
   [ol.llx.ai.impl.utils.tool-validation :as sut]))

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
               (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                 e))]
      (is (some? ex))
      (is (= :ol.llx/tool-not-found (-> ex ex-data :type)))
      (is (= "unknown-tool" (-> ex ex-data :tool-name)))
      (is (= ["search"] (-> ex ex-data :available-tools)))))
  (testing "validation errors are structured"
    (let [ex (try
               (sut/validate-tool-call tools {:name "search" :arguments {:q 123}})
               nil
               (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                 e))]
      (is (some? ex))
      (is (= :ol.llx/validation-error (-> ex ex-data :type)))
      (is (= "search" (-> ex ex-data :tool-name)))
      (is (map? (-> ex ex-data :errors)))
      (is (= {:q 123} (-> ex ex-data :arguments)))))
  (testing "missing required field"
    (let [ex (try
               (sut/validate-tool-call tools {:name "search" :arguments {}})
               nil
               (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                 e))]
      (is (some? ex))
      (is (map? (-> ex ex-data :errors)))
      (is (some? (-> ex ex-data :errors :q)))))
  (testing "human-readable error message includes tool name and arguments"
    (let [ex (try
               (sut/validate-tool-call tools {:name "search" :arguments {:q 123}})
               nil
               (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e e))]
      (is (some? ex))
      (is (str/includes? (ex-message ex) "search")
          "error message should mention tool name")
      (is (str/includes? (ex-message ex) "Validation failed")
          "error message should include 'Validation failed' prefix")
      (is (str/includes? (ex-message ex) "Received arguments")
          "error message should include 'Received arguments' section")))
  (testing "tool-not-found error message includes tool name"
    (let [ex (try
               (sut/validate-tool-call tools {:name "unknown" :arguments {}})
               nil
               (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e e))]
      (is (some? ex))
      (is (str/includes? (ex-message ex) "unknown")
          "error message should mention unknown tool name"))))
