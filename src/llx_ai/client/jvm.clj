(ns llx-ai.client.jvm
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [llx-ai.client :as client]))

(defn default-env
  []
  {:http/request (fn [request] (http/request request))
   :json/encode  json/write-str
   :json/decode  (fn [s _opts] (json/read-str s {:key-fn keyword}))
   :clock/now-ms (fn [] (System/currentTimeMillis))
   :id/new       (fn [] (str (java.util.UUID/randomUUID)))
   :env/get      (fn [k] (System/getenv k))})

(defn complete
  ([model context opts]
   (complete (default-env) model context opts))
  ([env model context opts]
   (client/complete env model context opts)))
