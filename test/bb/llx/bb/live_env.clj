(ns llx.bb.live-env
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def provider-services
  {"OLLAMA_API_KEY"    :llx/ollama
   "OPENAI_API_KEY"    :llx/openai
   "ANTHROPIC_API_KEY" :llx/anthropic
   "GEMINI_API_KEY"    :llx/google
   "MISTRAL_API_KEY"   :llx/mistral})

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
      (let [[k v] (str/split line (re-pattern "=") 2)
            k     (str/trim (or k ""))
            v     (str/trim (or v ""))]
        (when (seq k)
          [k (strip-wrapping-quotes v)])))))

(defn read-dotenv
  []
  (let [dotenv-path (some (fn [p]
                            (when (.exists (io/file p)) p))
                          [".env" "llx/.env"])]
    (if dotenv-path
      (with-open [reader (io/reader dotenv-path)]
        (reduce (fn [acc line]
                  (if-let [[k v] (parse-dotenv-line line)]
                    (assoc acc k v)
                    acc))
                {}
                (line-seq reader)))
      {})))

(defn clojure-extra-env
  "Returns dotenv vars that are not already present in the current process env.
   Intended for babashka.tasks/clojure `:extra-env` so subprocesses get dotenv
   values without overwriting exported environment variables."
  []
  (let [dotenv-values (read-dotenv)]
    (reduce-kv (fn [acc k v]
                 (if (some? (System/getenv k))
                   acc
                   (assoc acc k v)))
               {}
               dotenv-values)))

(defn provider-skip-meta-flags
  []
  (let [dotenv-values (read-dotenv)
        lookup-env    (fn [env-var]
                        (or (System/getenv env-var)
                            (get dotenv-values env-var)))
        enabled?      (fn [env-var]
                        (let [value (lookup-env env-var)]
                          (and (some? value)
                               (or (= env-var "OLLAMA_API_KEY")
                                   (seq value)))))]
    (->> provider-services
         (remove (fn [[env-var _]]
                   (enabled? env-var)))
         (mapcat (fn [[_ kw]]
                   ["--skip-meta" (str kw)])))))
