(ns llx.ai.oauth.openai-codex-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx.ai.impl.oauth.openai-codex :as sut]
   [llx.ai.impl.oauth.openai-codex-jvm :as jvm]))

(set! *warn-on-reflection* true)

(def account-token
  "aaa.eyJodHRwczovL2FwaS5vcGVuYWkuY29tL2F1dGgiOnsiY2hhdGdwdF9hY2NvdW50X2lkIjoiYWNjX3Rlc3QifX0.bbb")

(deftest parse-authorization-input-cases
  (testing "accepts full redirect URL"
    (is (= {:code "code-a" :state "state-a"}
           (sut/parse-authorization-input "https://auth.openai.com/callback?code=code-a&state=state-a"))))

  (testing "accepts code#state format"
    (is (= {:code "code-b" :state "state-b"}
           (sut/parse-authorization-input "code-b#state-b"))))

  (testing "accepts query-string format"
    (is (= {:code "code-c" :state "state-c"}
           (sut/parse-authorization-input "code=code-c&state=state-c"))))

  (testing "falls back to raw code"
    (is (= {:code "just-a-code"}
           (sut/parse-authorization-input "just-a-code")))))

(deftest account-id-extraction
  (is (= "acc_test" (sut/account-id-from-access-token account-token)))
  (is (nil? (sut/account-id-from-access-token "aaa.invalid.bbb"))))

(deftest login-openai-codex-manual-fallback
  (let [close-called* (atom false)
        result        (sut/login-openai-codex
                       {:on-auth              (fn [_auth-info])
                        :on-prompt            (fn [_prompt] "manual-code#state-1")
                        :on-manual-code-input (fn [] "manual-code#state-1")}
                       {:create-authorization-flow    (fn []
                                                        {:verifier "verifier-1"
                                                         :state    "state-1"
                                                         :url      "https://auth.example"})
                        :start-local-oauth-server     (fn [_state]
                                                        {:close         (fn [] (reset! close-called* true))
                                                         :cancel-wait   (fn [])
                                                         :wait-for-code (fn [] nil)})
                        :exchange-authorization-code  (fn [code verifier _redirect-uri]
                                                        (is (= "manual-code" code))
                                                        (is (= "verifier-1" verifier))
                                                        {:type    :success
                                                         :access  account-token
                                                         :refresh "refresh-1"
                                                         :expires 8000})
                        :account-id-from-access-token sut/account-id-from-access-token})]
    (is (= "acc_test" (:account-id result)))
    (is (= "refresh-1" (:refresh result)))
    (is (true? @close-called*))))

(deftest login-openai-codex-state-mismatch-fails
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"State mismatch"
       (sut/login-openai-codex
        {:on-auth              (fn [_auth-info])
         :on-prompt            (fn [_prompt] "manual-code#wrong-state")
         :on-manual-code-input (fn [] "manual-code#wrong-state")}
        {:create-authorization-flow    (fn []
                                         {:verifier "verifier-1"
                                          :state    "state-1"
                                          :url      "https://auth.example"})
         :start-local-oauth-server     (fn [_state]
                                         {:close         (fn [])
                                          :cancel-wait   (fn [])
                                          :wait-for-code (fn [] nil)})
         :exchange-authorization-code  (fn [_code _verifier _redirect-uri]
                                         {:type :failed})
         :account-id-from-access-token sut/account-id-from-access-token}))))

(deftest login-openai-codex-bind-failure-falls-back-to-manual-input
  (with-open [_socket (java.net.ServerSocket. 1455
                                              50
                                              (java.net.InetAddress/getByName "127.0.0.1"))]
    (let [result (sut/login-openai-codex
                  {:on-auth              (fn [_auth-info])
                   :on-prompt            (fn [_prompt] "bind-fallback-code#state-bind")
                   :on-manual-code-input (fn [] "bind-fallback-code#state-bind")}
                  {:create-authorization-flow    (fn []
                                                   {:verifier "verifier-bind"
                                                    :state    "state-bind"
                                                    :url      "https://auth.example"})
                   :start-local-oauth-server     jvm/start-local-oauth-server
                   :exchange-authorization-code  (fn [code verifier _redirect-uri]
                                                   (is (= "bind-fallback-code" code))
                                                   (is (= "verifier-bind" verifier))
                                                   {:type    :success
                                                    :access  account-token
                                                    :refresh "refresh-bind"
                                                    :expires 9000})
                   :account-id-from-access-token sut/account-id-from-access-token})]
      (is (= "acc_test" (:account-id result)))
      (is (= "refresh-bind" (:refresh result))))))
