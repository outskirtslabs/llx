(ns llx.cljs-test-runner
  (:require
   [lambdaisland.chui.remote]
   [lambdaisland.chui.test-data :as test-data])
  (:require-macros [lambdaisland.chui.test-data :refer [capture-test-data!]]))

(defn ^:export main
  [& _]
  (capture-test-data!)
  @test-data/test-ns-data)
