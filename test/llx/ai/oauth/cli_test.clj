(ns llx.ai.oauth.cli-test
  (:require
   [babashka.json :as json]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [llx.ai.oauth.cli :as sut]))

(set! *warn-on-reflection* true)

(defn- with-temp-dir
  [f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "llx-oauth-cli-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (f dir)
      (finally
        (doseq [file (reverse (file-seq dir))]
          (.delete ^java.io.File file))))))

(deftest login-command-writes-auth-json-in-current-working-directory
  (with-temp-dir
    (fn [^java.io.File dir]
      (let [credentials  {:access     "access-token"
                          :refresh    "refresh-token"
                          :expires    424242
                          :account-id "acc_test"}
            auth-file    (io/file dir "auth.json")
            original-pwd (System/getProperty "user.dir")]
        (with-redefs [sut/find-provider (fn [provider-id]
                                          (when (= "openai-codex" provider-id)
                                            {:id    "openai-codex"
                                             :name  "OpenAI Codex"
                                             :login (fn [_callbacks] credentials)}))]
          (try
            (System/setProperty "user.dir" (.getAbsolutePath ^java.io.File dir))
            (#'sut/login-command "openai-codex")
            (testing "persists oauth credentials to auth.json"
              (is (.exists auth-file))
              (is (= {"openai-codex" {"type"       "oauth"
                                      "access"     "access-token"
                                      "refresh"    "refresh-token"
                                      "expires"    424242
                                      "account-id" "acc_test"}}
                     (json/read-str (slurp auth-file) {:key-fn str}))))
            (finally
              (System/setProperty "user.dir" original-pwd))))))))
