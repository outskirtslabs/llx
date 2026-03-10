(ns llx.ai.oauth-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [llx.ai.oauth :as sut]))

#?(:clj (set! *warn-on-reflection* true))

(defn- provider
  [id]
  {:id                    id
   :name                  (str "Provider " id)
   :uses-callback-server? true
   :login                 (fn [_callbacks]
                            {:access  "login-access"
                             :refresh "login-refresh"
                             :expires 5000})
   :refresh-token         (fn [credentials]
                            (assoc credentials
                                   :access "refreshed-access"
                                   :expires 10000))
   :get-api-key           (fn [credentials]
                            (:access credentials))})

(deftest oauth-provider-registry-operations
  (let [provider-id "test-oauth-provider"
        p           (provider provider-id)]
    (sut/register-oauth-provider! p)
    (is (= p (sut/get-oauth-provider provider-id)))
    (is (some #(= provider-id (:id %))
              (sut/get-oauth-providers)))))

(deftest get-oauth-api-key-behavior
  (let [provider-id "test-oauth-provider-refresh"
        p           (provider provider-id)
        now-ms      (constantly 3000)]
    (sut/register-oauth-provider! p)

    (testing "returns nil for missing provider credentials"
      (is (nil? (sut/get-oauth-api-key provider-id {} {:now-ms now-ms}))))

    (testing "returns existing api key when credential is not expired"
      (let [result (sut/get-oauth-api-key provider-id
                                          {provider-id {:access  "existing"
                                                        :refresh "r1"
                                                        :expires 5000}}
                                          {:now-ms now-ms})]
        (is (= "existing" (:api-key result)))
        (is (= "existing" (get-in result [:new-credentials :access])))))

    (testing "refreshes expired credential before returning api key"
      (let [result (sut/get-oauth-api-key provider-id
                                          {provider-id {:access  "stale"
                                                        :refresh "r2"
                                                        :expires 2000}}
                                          {:now-ms now-ms})]
        (is (= "refreshed-access" (:api-key result)))
        (is (= "refreshed-access" (get-in result [:new-credentials :access])))
        (is (> (get-in result [:new-credentials :expires]) 3000))))))

(deftest refresh-oauth-token-dispatch
  (let [provider-id "test-oauth-provider-dispatch"
        p           (provider provider-id)
        _           (sut/register-oauth-provider! p)
        refreshed   (sut/refresh-oauth-token provider-id
                                             {:access  "stale"
                                              :refresh "r3"
                                              :expires 1})]
    (is (= "refreshed-access" (:access refreshed)))))
