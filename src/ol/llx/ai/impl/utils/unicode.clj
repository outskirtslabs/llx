(ns ol.llx.ai.impl.utils.unicode
  (:require
   [clojure.walk :as walk]))

(defn sanitize-surrogates
  "Removes unpaired UTF-16 surrogate code units from `text`.

  Preserves valid surrogate pairs (for example, emoji) and removes
  standalone high/low surrogates that can break JSON serialization."
  [text]
  (let [s   (str (or text ""))
        len (.length ^String s)]
    (loop [i  0
           sb (StringBuilder.)]
      (if (>= i len)
        (.toString sb)
        (let [ch (.charAt ^String s i)]
          (cond
            (Character/isHighSurrogate ch)
            (if (< (inc i) len)
              (let [next-ch (.charAt ^String s (inc i))]
                (if (Character/isLowSurrogate next-ch)
                  (do
                    (.append sb ch)
                    (.append sb next-ch)
                    (recur (+ i 2) sb))
                  (recur (inc i) sb)))
              (recur (inc i) sb))

            (Character/isLowSurrogate ch)
            (recur (inc i) sb)

            :else
            (do
              (.append sb ch)
              (recur (inc i) sb))))))))

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
