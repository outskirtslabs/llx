(ns llx.ai.tests-main
  (:require
   [clojure.test :as t]
   [llx.ai.default-env-test]))

(enable-console-print!)

(set! *main-cli-fn*
      #(t/run-tests
        'llx.ai.default-env-test))

(defmethod t/report [:cljs.test/default :end-run-tests]
  [m]
  (if (t/successful? m)
    (set! (.-exitCode js/process) 0)
    (set! (.-exitCode js/process) 1)))
