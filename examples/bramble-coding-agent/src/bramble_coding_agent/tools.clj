(ns bramble-coding-agent.tools
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.nio.file Files]
   [java.util Base64]
   [java.util.concurrent TimeUnit]))

(def ^:private max-lines 2000)
(def ^:private max-bytes (* 50 1024))

(defn truncate-head
  [text]
  (let [all-lines   (str/split-lines text)
        total-lines (count all-lines)]
    (loop [i 0 out [] bytes 0]
      (cond
        (= i total-lines)
        {:content      (str/join "\n" out)
         :truncated    false
         :total-lines  total-lines
         :output-lines (count out)
         :truncated-by nil}

        (>= i max-lines)
        {:content      (str/join "\n" out)
         :truncated    true
         :total-lines  total-lines
         :output-lines i
         :truncated-by :lines}

        :else
        (let [line      (nth all-lines i)
              lb        (+ (count (.getBytes ^String line "UTF-8")) (if (> i 0) 1 0))
              new-bytes (+ bytes lb)]
          (if (> new-bytes max-bytes)
            {:content      (str/join "\n" out)
             :truncated    true
             :total-lines  total-lines
             :output-lines i
             :truncated-by :bytes}
            (recur (inc i) (conj out line) new-bytes)))))))

(defn truncate-tail
  [text]
  (let [all-lines   (str/split-lines text)
        total-lines (count all-lines)]
    (loop [i (dec total-lines) out [] bytes 0]
      (cond
        (< i 0)
        {:content      (str/join "\n" out)
         :truncated    false
         :total-lines  total-lines
         :output-lines (count out)
         :truncated-by nil}

        (>= (count out) max-lines)
        {:content      (str/join "\n" out)
         :truncated    true
         :total-lines  total-lines
         :output-lines (count out)
         :truncated-by :lines}

        :else
        (let [line      (nth all-lines i)
              lb        (+ (count (.getBytes ^String line "UTF-8")) (if (seq out) 1 0))
              new-bytes (+ bytes lb)]
          (if (> new-bytes max-bytes)
            {:content      (str/join "\n" out)
             :truncated    true
             :total-lines  total-lines
             :output-lines (count out)
             :truncated-by :bytes}
            (recur (dec i) (into [line] out) new-bytes)))))))

(def ^:private image-mime-types
  {"jpg"  "image/jpeg"
   "jpeg" "image/jpeg"
   "png"  "image/png"
   "gif"  "image/gif"
   "webp" "image/webp"})

(defn- file-extension [path]
  (let [name (.getName (io/file path))]
    (when-let [dot (str/last-index-of name ".")]
      (str/lower-case (subs name (inc dot))))))

(defn- image-mime-type [path]
  (get image-mime-types (file-extension path)))

(defn- detect-line-ending [text]
  (if (str/includes? text "\r\n") "\r\n" "\n"))

(defn- normalize-to-lf [text]
  (-> text
      (str/replace "\r\n" "\n")
      (str/replace "\r" "\n")))

(defn- restore-line-endings [text ending]
  (if (= ending "\r\n")
    (str/replace text "\n" "\r\n")
    text))

(defn- strip-bom [text]
  (if (str/starts-with? text "\uFEFF")
    {:bom "\uFEFF" :text (subs text 1)}
    {:bom "" :text text}))

(defn- normalize-for-fuzzy [text]
  (-> (->> (str/split-lines text)
           (map str/trimr)
           (str/join "\n"))
      (str/replace #"[\u2018\u2019\u201A\u201B]" "'")
      (str/replace #"[\u201C\u201D\u201E\u201F]" "\"")
      (str/replace #"[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]" "-")
      (str/replace #"[\u00A0\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000]" " ")))

(defn- fuzzy-find-text [content old-text]
  (let [exact-idx (str/index-of content old-text)]
    (if (and exact-idx (>= exact-idx 0))
      {:found                   true
       :index                   exact-idx
       :match-length            (count old-text)
       :content-for-replacement content}
      (let [fuzzy-content (normalize-for-fuzzy content)
            fuzzy-old     (normalize-for-fuzzy old-text)
            fuzzy-idx     (str/index-of fuzzy-content fuzzy-old)]
        (if (and fuzzy-idx (>= fuzzy-idx 0))
          {:found                   true
           :index                   fuzzy-idx
           :match-length            (count fuzzy-old)
           :content-for-replacement fuzzy-content}
          {:found                   false
           :index                   -1
           :match-length            0
           :content-for-replacement content})))))

(defn execute-read
  [_id {:keys [path offset limit]} _abort _on-update]
  (let [f (io/file path)]
    (cond
      (not (.exists f))
      (throw (ex-info (str "File not found: " path) {:type :not-found}))

      (not (.isFile f))
      (throw (ex-info (str "Not a regular file: " path) {:type :not-a-file}))

      :else
      (if-let [mime (image-mime-type path)]
        (let [bytes (Files/readAllBytes (.toPath f))
              b64   (.encodeToString (Base64/getEncoder) bytes)]
          {:content [{:type :text :text (str "Read image " path " [" mime "].")}
                     {:type :image :data b64 :mime-type mime}]})
        (let [all-lines   (str/split-lines (slurp f))
              total-lines (count all-lines)
              start       (if offset (max 0 (dec offset)) 0)]
          (when (and offset (> offset total-lines))
            (throw (ex-info (str "Offset " offset " is beyond end of file (" total-lines " lines total)")
                            {:type :offset-out-of-bounds})))
          (let [selected    (cond->> (drop start all-lines)
                              limit (take limit))
                text        (str/join "\n" selected)
                result      (truncate-head text)
                next-offset (+ (inc start) (:output-lines result))
                output-text (if (:truncated result)
                              (str (:content result)
                                   "\n\n[Showing lines " (inc start) "-"
                                   (+ start (:output-lines result))
                                   " of " total-lines
                                   ". Use offset=" next-offset " to continue.]")
                              (:content result))]
            {:content [{:type :text :text output-text}]}))))))

(defn execute-write
  [_id {:keys [path content]} _abort _on-update]
  (let [f      (io/file path)
        parent (.getParentFile f)]
    (when (and parent (not (.exists parent)))
      (.mkdirs parent))
    (spit f content)
    {:content [{:type :text
                :text (str "Successfully wrote "
                           (count (.getBytes ^String content "UTF-8"))
                           " bytes to " path)}]}))

(defn execute-edit
  [_id {:keys [path oldText newText]} _abort _on-update]
  (let [f (io/file path)]
    (when-not (.exists f)
      (throw (ex-info (str "File not found: " path) {:type :not-found})))
    (let [raw                                                        (slurp f)
          {:keys [bom text]}                                         (strip-bom raw)
          line-ending                                                (detect-line-ending text)
          norm-content                                               (normalize-to-lf text)
          norm-old                                                   (normalize-to-lf oldText)
          norm-new                                                   (normalize-to-lf newText)
          {:keys [found index match-length content-for-replacement]}
          (fuzzy-find-text norm-content norm-old)]
      (when-not found
        (throw (ex-info (str "Could not find the exact text in " path
                             ". The old text must match exactly including all whitespace and newlines.")
                        {:type :not-found})))
      (let [fuzzy-content (normalize-for-fuzzy norm-content)
            fuzzy-old     (normalize-for-fuzzy norm-old)
            occurrences   (count (re-seq (re-pattern (java.util.regex.Pattern/quote fuzzy-old))
                                         fuzzy-content))]
        (when (> occurrences 1)
          (throw (ex-info (str "Found " occurrences " occurrences of the text in " path
                               ". The text must be unique. Please provide more context to make it unique.")
                          {:type :ambiguous})))
        (let [new-content (str (subs content-for-replacement 0 index)
                               norm-new
                               (subs content-for-replacement (+ index match-length)))]
          (when (= content-for-replacement new-content)
            (throw (ex-info (str "No changes made to " path
                                 ". The replacement produced identical content.")
                            {:type :no-change})))
          (spit f (str bom (restore-line-endings new-content line-ending)))
          {:content [{:type :text :text (str "Successfully replaced text in " path ".")}]})))))

(defn execute-bash
  [_id {:keys [command timeout]} _abort on-update]
  (let [cwd       (System/getProperty "user.dir")
        pb        (doto (ProcessBuilder. (into-array String ["bash" "-c" command]))
                    (.directory (io/file cwd))
                    (.redirectErrorStream true))
        proc      (.start pb)
        in-stream (.getInputStream proc)
        sb        (StringBuilder.)
        done-p    (promise)]
    (doto (Thread.
           (fn []
             (try
               (let [buf (byte-array 8192)]
                 (loop []
                   (let [n (.read in-stream buf)]
                     (when (> n 0)
                       (locking sb
                         (.append sb (String. buf 0 n "UTF-8")))
                       (when on-update
                         (on-update {:content [{:type :text
                                                :text (locking sb (str sb))}]}))
                       (recur)))))
               (finally
                 (deliver done-p true)))))
      (.setDaemon true)
      (.start))
    (when-not (if timeout
                (.waitFor proc (long timeout) TimeUnit/SECONDS)
                (do (.waitFor proc) true))
      (.destroyForcibly proc)
      (deref done-p 2000 nil)
      (let [partial (locking sb (str sb))]
        (throw (ex-info (str (when (seq partial) (str partial "\n\n"))
                             "Command timed out after " timeout " seconds")
                        {:type :timeout}))))
    (deref done-p 5000 nil)
    (let [exit-code                               (.exitValue proc)
          output                                  (locking sb (str sb))
          {:keys [content truncated total-lines]} (truncate-tail output)
          output-text                             (cond-> (if (seq content) content "(no output)")
                                                    truncated (str "\n\n[Output truncated. "
                                                                   total-lines " total lines.]"))]
      (if (not= exit-code 0)
        (throw (ex-info (str output-text "\n\nCommand exited with code " exit-code)
                        {:type :nonzero-exit :exit-code exit-code}))
        {:content [{:type :text :text output-text}]}))))

(defn make-tools []
  [{:name         "read"
    :description  (str "Read the contents of a file. Supports text files and images "
                       "(jpg, png, gif, webp). Images are sent as attachments. "
                       "For text files, output is truncated to " max-lines " lines or "
                       (/ max-bytes 1024) "KB. Use offset/limit for large files.")
    :input-schema [:map
                   [:path :string]
                   [:offset {:optional true} :int]
                   [:limit {:optional true} :int]]
    :execute      execute-read}
   {:name         "write"
    :description  "Write content to a file. Creates parent directories if needed."
    :input-schema [:map
                   [:path :string]
                   [:content :string]]
    :execute      execute-write}
   {:name         "edit"
    :description  "Edit a file by replacing exact text with strict uniqueness checks."
    :input-schema [:map
                   [:path :string]
                   [:oldText :string]
                   [:newText :string]]
    :execute      execute-edit}
   {:name         "bash"
    :description  (str "Execute a bash command in the current working directory. "
                       "Returns stdout and stderr with tail truncation. "
                       "Optionally provide a timeout in seconds.")
    :input-schema [:map
                   [:command :string]
                   [:timeout {:optional true} :double]]
    :execute      execute-bash}])
