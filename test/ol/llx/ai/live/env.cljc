(ns ol.llx.ai.live.env
  (:require
   #?@(:clj [[clojure.java.io :as io]]
       :cljs [["node:fs" :as fs]])
   [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(defn- strip-wrapping-quotes
  [s]
  (if (and (string? s)
           (>= (count s) 2)
           (= (first s) (last s))
           (or (= (first s) \")
               (= (first s) \')))
    (subs s 1 (dec (count s)))
    s))

(defn- parse-dotenv-line
  [line]
  (let [line (str/trim line)]
    (when (and (seq line) (not (str/starts-with? line "#")))
      (let [[k v] (str/split line #"=" 2)
            k     (str/trim (or k ""))
            v     (str/trim (or v ""))]
        (when (seq k)
          [k (strip-wrapping-quotes v)])))))

(defn- load-dotenv
  []
  #?(:clj
     (let [dotenv-file (java.io.File. ".env")]
       (if (.exists dotenv-file)
         (with-open [reader (io/reader dotenv-file)]
           (reduce (fn [acc line]
                     (if-let [[k v] (parse-dotenv-line line)]
                       (assoc acc k v)
                       acc))
                   {}
                   (line-seq reader)))
         {}))
     :cljs
     (if (.existsSync fs ".env")
       (reduce (fn [acc line]
                 (if-let [[k v] (parse-dotenv-line line)]
                   (assoc acc k v)
                   acc))
               {}
               (str/split-lines (.readFileSync fs ".env" "utf8")))
       {})))

(defonce dotenv-values* (delay (load-dotenv)))

(defn get-env
  [k]
  (or #?(:clj (System/getenv k)
         :cljs (some-> js/process .-env (aget k)))
      (get @dotenv-values* k)))
