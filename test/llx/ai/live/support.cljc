(ns llx.ai.live.support
  (:require
   #?@(:cljs [[cljs.test :refer [is]]
              ["node:fs" :as fs]])
   [clojure.string :as str]
   [llx.ai.test-util :as util]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn now-ms
  []
  (util/now-ms))

(defn collect-events*
  [ch timeout-ms]
  (let [deadline-ms   (+ (now-ms) timeout-ms)
        timeout-token #?(:clj (Object.) :cljs (js-obj))]
    (p/loop [events []]
      (let [remaining-ms (- deadline-ms (now-ms))]
        (if (<= remaining-ms 0)
          ::timeout
          (p/let [event (p/race [(sp/take ch)
                                 (p/delay remaining-ms timeout-token)])]
            (cond
              (identical? event timeout-token)
              ::timeout

              (nil? event)
              events

              :else
              (p/recur (conj events event)))))))))

(defn collect-stream*
  [ch timeout-ms]
  (-> (collect-events* ch timeout-ms)
      (p/then (fn [events]
                (if (= ::timeout events)
                  {:events events :result ::timeout}
                  {:events events
                   :result (:assistant-message (last events))})))))

(defn read-file-base64
  [path]
  #?(:clj (let [bytes (java.nio.file.Files/readAllBytes (.toPath (java.io.File. path)))]
            (.encodeToString (java.util.Base64/getEncoder) bytes))
     :cljs (-> (.readFileSync fs path)
               (.toString "base64"))))

(defn run-with-timeout*
  [deferred timeout-ms]
  (p/race [deferred
           (p/delay timeout-ms ::timeout)]))

(defn timeout-result?
  [x]
  (= ::timeout x))

(defn env-set?
  [value]
  (boolean (and (string? value)
                (not (str/blank? value)))))

#?(:cljs
   (defn fail-and-done!
     [done err]
     (is nil (str err))
     (done)))
