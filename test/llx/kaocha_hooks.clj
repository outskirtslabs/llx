(ns llx.kaocha-hooks
  "Kaocha CLJS helpers for starting and reusing a shadow-cljs node client."
  (:require
   [clojure.java.shell :as shell]
   [kaocha.cljs2.funnel-client :as funnel]
   [kaocha.type.cljs2 :as cljs2-type]
   [shadow.cljs.devtools.api :as shadow-api]))

(set! *warn-on-reflection* true)

(defonce node-client* (atom nil))

(defn- stop-node-client!
  []
  (when-let [{:keys [process]} @node-client*]
    (try
      (.destroy ^Process process)
      (catch Exception _))
    (reset! node-client* nil)))

(defonce shutdown-hook-installed?
  (delay
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable stop-node-client!))))

(defn- suite-id
  [suite]
  (or (:kaocha.testable/id suite)
      (:id suite)))

(defn- suite->build
  [suite]
  (case (suite-id suite)
    :cljs-live {:build-id  :kaocha-test-live
                :output-to "target/kaocha-tests-live.js"}
    {:build-id  :kaocha-test
     :output-to "target/kaocha-tests.js"}))

(defn- start-node-client!
  [suite]
  (try
    (shell/sh "pkill"
              "-f"
              (str (System/getProperty "user.dir") "/target/kaocha-tests"))
    (catch Exception _))
  (stop-node-client!)
  (let [{:keys [build-id output-to]} (suite->build suite)
        _                            (shadow-api/compile! build-id {})
        ^"[Ljava.lang.String;" cmd   (into-array String ["node" output-to])
        process                      (.start (ProcessBuilder. cmd))]
    (reset! node-client* {:process process :build-id build-id})
    process))

(defn cljs2-clients-hook
  "Clients hook for :kaocha.type/cljs2 suites that boots a shadow node client
  when needed and then delegates to kaocha-cljs2's default client discovery.
  "
  [{:funnel/keys [conn] :as suite}]
  @shutdown-hook-installed?
  (let [existing-ids (->> (funnel/list-clients conn)
                          (map :id)
                          set)]
    (start-node-client! suite)
    (let [clients (cljs2-type/default-clients-hook suite)
          fresh   (remove (comp existing-ids :id) clients)]
      (if (seq fresh) fresh clients))))
