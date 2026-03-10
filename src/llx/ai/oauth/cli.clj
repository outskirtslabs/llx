(ns llx.ai.oauth.cli
  (:require
   [babashka.json :as json]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [llx.ai.oauth :as oauth]))

(set! *warn-on-reflection* true)

(def ^:private auth-file-name
  "auth.json")

(defn- usage
  []
  (println "Usage:")
  (println "  clojure -M:dev -m llx.ai.oauth.cli providers")
  (println "  clojure -M:dev -m llx.ai.oauth.cli login <provider-id>"))

(defn- auth-file
  []
  (io/file (System/getProperty "user.dir") auth-file-name))

(defn- load-auth
  []
  (let [file (auth-file)]
    (if (.exists ^java.io.File file)
      (try
        (json/read-str (slurp file) {:key-fn str})
        (catch Exception _
          {}))
      {})))

(defn- save-auth!
  [auth]
  (let [file (auth-file)]
    (spit file (json/write-str auth {:indent true}))
    file))

(defn- prompt!
  [message]
  (println message)
  (flush)
  (or (read-line) ""))

(defn- find-provider
  [provider-id]
  (oauth/get-oauth-provider provider-id))

(defn- providers-command
  []
  (doseq [provider (oauth/get-oauth-providers)]
    (println (:id provider) "-" (:name provider)))
  nil)

(defn- login-command
  [provider-id]
  (if-let [provider (find-provider provider-id)]
    (let [credentials ((:login provider)
                       {:on-auth   (fn [{:keys [url instructions]}]
                                     (println instructions)
                                     (println)
                                     (println "Open:")
                                     (println url)
                                     (println))
                        :on-prompt (fn [{:keys [message]}]
                                     (prompt! message))})]
      (println "Credentials:")
      (pprint/pprint credentials)
      (save-auth! (assoc (load-auth)
                         provider-id
                         (assoc credentials :type "oauth")))
      (println "Credentials saved to" auth-file-name)
      credentials)
    (do
      (println "Unknown OAuth provider:" provider-id)
      (usage)
      nil)))

(defn -main
  [& args]
  (let [[command provider-id] args]
    (case command
      "providers" (providers-command)
      "login" (if provider-id
                (login-command provider-id)
                (usage))
      (usage))))
