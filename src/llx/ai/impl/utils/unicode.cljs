(ns llx.ai.impl.utils.unicode
  (:require
   [clojure.walk :as walk]))

(defn- high-surrogate?
  [code]
  (<= 55296 code 56319))

(defn- low-surrogate?
  [code]
  (<= 56320 code 57343))

(defn sanitize-surrogates
  "Removes unpaired UTF-16 surrogate code units from `text`.

  Preserves valid surrogate pairs and removes standalone high/low surrogates."
  [text]
  (let [s   (str (or text ""))
        len (.-length s)]
    (loop [i   0
           out []]
      (if (>= i len)
        (apply str out)
        (let [ch (.charCodeAt s i)]
          (cond
            (high-surrogate? ch)
            (if (< (inc i) len)
              (let [next-ch (.charCodeAt s (inc i))]
                (if (low-surrogate? next-ch)
                  (recur (+ i 2) (conj out (.charAt s i) (.charAt s (inc i))))
                  (recur (inc i) out)))
              (recur (inc i) out))

            (low-surrogate? ch)
            (recur (inc i) out)

            :else
            (recur (inc i) (conj out (.charAt s i)))))))))

(defn sanitize-payload
  "Recursively walks a data structure, applying `sanitize-surrogates` to all strings."
  [data]
  (walk/postwalk #(if (string? %) (sanitize-surrogates %) %) data))

(defn truncate
  "Returns `s` truncated to at most `max-len` characters."
  [s max-len]
  (let [s (or s "")]
    (if (> (count s) max-len)
      (subs s 0 max-len)
      s)))
