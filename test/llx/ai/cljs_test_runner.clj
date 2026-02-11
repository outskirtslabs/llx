(ns llx.ai.cljs-test-runner
  (:require
   [cljs.build.api :as cljs]
   [clojure.java.shell :as shell]))

(def build-options
  {:main          'llx.ai.tests-main
   :output-to     "target/cljs-tests.js"
   :output-dir    "target/cljs-tests"
   :source-map    true
   :target        :nodejs
   :optimizations :none
   :pretty-print  true
   :pseudo-names  true
   :verbose       false})

(defn -main
  [& _args]
  (cljs/build (cljs/inputs "src" "test") build-options)
  (let [{:keys [out err exit]} (shell/sh "node" "target/cljs-tests.js")]
    (when (seq out)
      (print out))
    (when (seq err)
      (binding [*out* *err*]
        (print err)))
    (System/exit exit)))
