(ns llx-ai.live.env
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

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
  (let [dotenv-file (io/file ".env")]
    (if (.exists dotenv-file)
      (with-open [reader (io/reader dotenv-file)]
        (reduce (fn [acc line]
                  (if-let [[k v] (parse-dotenv-line line)]
                    (assoc acc k v)
                    acc))
                {}
                (line-seq reader)))
      {})))

(defonce dotenv-values* (delay (load-dotenv)))

(defn get-env
  [k]
  (or (System/getenv k)
      (get @dotenv-values* k)))
