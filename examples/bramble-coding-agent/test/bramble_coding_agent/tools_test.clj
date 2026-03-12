(ns bramble-coding-agent.tools-test
  (:require
   [bramble-coding-agent.tools :as sut]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]])
  (:import
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]))

(def ^:private ^:dynamic *tmp-dir* nil)

(defn- tmp-fixture [f]
  (let [dir (Files/createTempDirectory "llx-bramble-tools-test" (make-array FileAttribute 0))]
    (binding [*tmp-dir* (.toFile dir)]
      (try
        (f)
        (finally
          (doseq [file (reverse (file-seq *tmp-dir*))]
            (.delete file)))))))

(use-fixtures :each tmp-fixture)

(defn- tmp-file
  ([] (io/file *tmp-dir* (str (random-uuid) ".txt")))
  ([name] (io/file *tmp-dir* name)))

(defn- write-tmp [content]
  (let [f (tmp-file)]
    (spit f content)
    f))

(defn- call [execute-fn args]
  (execute-fn "test-id" args nil nil))

(defn- text-content [result]
  (->> (:content result)
       (filter #(= :text (:type %)))
       (map :text)
       (str/join "")))

(deftest truncate-head-no-truncation-test
  (let [r (sut/truncate-head "a\nb\nc")]
    (is (= "a\nb\nc" (:content r)))
    (is (false? (:truncated r)))
    (is (= 3 (:total-lines r)))
    (is (nil? (:truncated-by r)))))

(deftest truncate-tail-no-truncation-test
  (let [r (sut/truncate-tail "a\nb\nc")]
    (is (= "a\nb\nc" (:content r)))
    (is (false? (:truncated r)))
    (is (= 3 (:total-lines r)))))

(deftest read-text-file-with-offset-and-limit-test
  (let [f (write-tmp "line1\nline2\nline3\nline4")]
    (is (= "line2\nline3"
           (text-content (call sut/execute-read {:path   (.getAbsolutePath f)
                                                 :offset 2
                                                 :limit  2}))))))

(deftest read-text-file-offset-beyond-end-test
  (let [f (write-tmp "line1\nline2")]
    (is (thrown-with-msg? Exception #"beyond end of file"
                          (call sut/execute-read {:path (.getAbsolutePath f) :offset 5})))))

(deftest read-image-file-test
  (testing "image file returns text + image blocks"
    (let [png-bytes (byte-array [137 80 78 71 13 10 26 10
                                 0 0 0 13 73 72 68 82
                                 0 0 0 1 0 0 0 1 8 2
                                 0 0 0 -112 119 83 -34
                                 0 0 0 12 73 68 65 84
                                 8 -41 99 -8 -49 -64 0 0 0 2 0 1
                                 -27 39 -34 -4
                                 0 0 0 0 73 69 78 68 -82 66 96 -126])
          f         (io/file *tmp-dir* "test.png")]
      (Files/write (.toPath f) png-bytes (make-array java.nio.file.OpenOption 0))
      (let [result (:content (call sut/execute-read {:path (.getAbsolutePath f)}))]
        (is (= 2 (count result)))
        (is (= :image (:type (second result))))
        (is (= "image/png" (:mime-type (second result))))
        (is (seq (:data (second result))))))))

(deftest write-creates-parent-directories-test
  (let [f (io/file *tmp-dir* "a" "b" "c" "deep.txt")]
    (call sut/execute-write {:path (.getAbsolutePath f) :content "deep"})
    (is (.exists f))
    (is (= "deep" (slurp f)))))

(deftest edit-fuzzy-match-trailing-whitespace-test
  (let [f (write-tmp "hello   \nworld")]
    (call sut/execute-edit {:path    (.getAbsolutePath f)
                            :oldText "hello\nworld"
                            :newText "bye"})
    (is (= "bye" (slurp f)))))

(deftest edit-ambiguous-match-test
  (let [f (write-tmp "foo bar foo")]
    (is (thrown-with-msg? Exception #"occurrences"
                          (call sut/execute-edit {:path    (.getAbsolutePath f)
                                                  :oldText "foo"
                                                  :newText "baz"})))))

(deftest edit-preserves-crlf-line-endings-test
  (let [f   (write-tmp "line1\r\nline2\r\n")
        _   (call sut/execute-edit {:path    (.getAbsolutePath f)
                                    :oldText "line1"
                                    :newText "replaced"})
        out (slurp f)]
    (is (str/includes? out "\r\n"))
    (is (str/includes? out "replaced"))))

(deftest bash-stderr-merged-test
  (let [result (call sut/execute-bash {:command "echo out && echo err >&2"})
        out    (text-content result)]
    (is (str/includes? out "out"))
    (is (str/includes? out "err"))))

(deftest bash-nonzero-exit-includes-output-test
  (try
    (call sut/execute-bash {:command "echo before && exit 42"})
    (is false "should have thrown")
    (catch Exception e
      (is (str/includes? (ex-message e) "before"))
      (is (str/includes? (ex-message e) "42")))))

(deftest bash-timeout-throws-test
  (is (thrown-with-msg? Exception #"timed out"
                        (call sut/execute-bash {:command "sleep 10" :timeout 1}))))
