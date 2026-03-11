(ns ol.llx.ai.live.openai-codex-oauth-smoke-test
  (:require
   [babashka.json :as json]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [ol.llx.ai :as client]
   [ol.llx.ai.live.env :as live-env]
   [ol.llx.ai.live.models :as models]
   [ol.llx.ai.oauth :as oauth]
   [ol.llx.ai.test-util :as util]
   [promesa.core :as p]))

(set! *warn-on-reflection* true)

(def ^:private env
  (client/default-env))

(defn- auth-file-path
  []
  (or (live-env/get-env "LLX_LIVE_OPENAI_CODEX_AUTH_FILE")
      "auth.json"))

(defn- load-auth-file
  [path]
  (when (.exists (io/file path))
    (json/read-str (slurp path) {:key-fn keyword})))

(defn- oauth-api-key-result
  []
  (when-let [auth (load-auth-file (auth-file-path))]
    (oauth/get-oauth-api-key "openai-codex"
                             {"openai-codex" (get auth :openai-codex)})))

(deftest ^:ol.llx/openai live-openai-codex-oauth-smoke
  (if-let [oauth-result (oauth-api-key-result)]
    (util/async done
                (util/run-live-async!
                 (p/let [out (client/complete* env models/openai-codex
                                               {:messages [{:role      :user
                                                            :content   "reply with exactly: llx openai codex oauth smoke ok"
                                                            :timestamp 1}]}
                                               {:api-key           (:api-key oauth-result)
                                                :max-output-tokens 96
                                                :reasoning         {:effort :high}})]
                   (is (= :assistant (:role out)))
                   (is (seq (:content out)))
                   true)
                 done))
    (is true (str "Skipping OpenAI Codex OAuth smoke test; missing " (auth-file-path)))))
