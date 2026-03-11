(ns minimal-coding-agent.tools-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [minimal-coding-agent.tools :as sut])
  (:import
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]))

(def ^:private ^:dynamic *tmp-dir* nil)

(defn- tmp-fixture [f]
  (let [dir (Files/createTempDirectory "llx-tools-test" (make-array FileAttribute 0))]
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

;; ─── truncate-head ───────────────────────────────────────────────────────────

(deftest truncate-head-no-truncation-test
  (let [r (sut/truncate-head "a\nb\nc")]
    (is (= "a\nb\nc" (:content r)))
    (is (false? (:truncated r)))
    (is (= 3 (:total-lines r)))
    (is (nil? (:truncated-by r)))))

(deftest truncate-head-empty-string-test
  (let [r (sut/truncate-head "")]
    (is (= "" (:content r)))
    (is (false? (:truncated r)))))

;; ─── truncate-tail ───────────────────────────────────────────────────────────

(deftest truncate-tail-no-truncation-test
  (let [r (sut/truncate-tail "a\nb\nc")]
    (is (= "a\nb\nc" (:content r)))
    (is (false? (:truncated r)))
    (is (= 3 (:total-lines r)))))

(deftest truncate-tail-empty-string-test
  (let [r (sut/truncate-tail "")]
    (is (= "" (:content r)))
    (is (false? (:truncated r)))))

;; ─── read tool ───────────────────────────────────────────────────────────────

(deftest read-text-file-test
  (let [f (write-tmp "hello\nworld")]
    (is (= "hello\nworld"
           (text-content (call sut/execute-read {:path (.getAbsolutePath f)}))))))

(deftest read-text-file-with-offset-test
  (let [f (write-tmp "line1\nline2\nline3\nline4")]
    (is (= "line2\nline3\nline4"
           (text-content (call sut/execute-read {:path (.getAbsolutePath f) :offset 2}))))))

(deftest read-text-file-with-limit-test
  (let [f (write-tmp "line1\nline2\nline3\nline4")]
    (is (= "line1\nline2"
           (text-content (call sut/execute-read {:path (.getAbsolutePath f) :limit 2}))))))

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

(deftest read-missing-file-test
  (is (thrown-with-msg? Exception #"File not found"
                        (call sut/execute-read {:path "/no/such/file/exists.txt"}))))

(deftest read-text-file-truncation-notice-test
  (testing "truncation notice included when file exceeds line limit"
    (let [lines  (str/join "\n" (map str (range 2500)))
          f      (write-tmp lines)
          result (text-content (call sut/execute-read {:path (.getAbsolutePath f)}))]
      (is (str/includes? result "continue")))))

(deftest read-image-file-test
  (testing "image file returns text + image blocks"
    ;; write a minimal 1x1 PNG (smallest valid PNG bytes)
    (let [png-bytes (byte-array [137 80 78 71 13 10 26 10    ; PNG signature
                                 0 0 0 13 73 72 68 82        ; IHDR chunk length + type
                                 0 0 0 1 0 0 0 1 8 2        ; 1x1 RGB
                                 0 0 0 -112 119 83 -34       ; crc
                                 0 0 0 12 73 68 65 84        ; IDAT
                                 8 -41 99 -8 -49 -64 0 0 0 2 0 1 ; compressed data
                                 -27 39 -34 -4               ; crc
                                 0 0 0 0 73 69 78 68 -82 66 96 -126]) ; IEND
          f         (io/file *tmp-dir* "test.png")]
      (Files/write (.toPath f) png-bytes (make-array java.nio.file.OpenOption 0))
      (let [result (:content (call sut/execute-read {:path (.getAbsolutePath f)}))]
        (is (= 2 (count result)))
        (is (= :text (:type (first result))))
        (is (str/includes? (:text (first result)) "image/png"))
        (is (= :image (:type (second result))))
        (is (= "image/png" (:mime-type (second result))))
        (is (seq (:data (second result))))))))

;; ─── write tool ──────────────────────────────────────────────────────────────

(deftest write-new-file-test
  (let [f      (tmp-file "newfile.txt")
        result (call sut/execute-write {:path (.getAbsolutePath f) :content "hello"})]
    (is (str/includes? (text-content result) "bytes"))
    (is (= "hello" (slurp f)))))

(deftest write-overwrites-existing-file-test
  (let [f (write-tmp "original")]
    (call sut/execute-write {:path (.getAbsolutePath f) :content "replaced"})
    (is (= "replaced" (slurp f)))))

(deftest write-creates-parent-directories-test
  (let [f (io/file *tmp-dir* "a" "b" "c" "deep.txt")]
    (call sut/execute-write {:path (.getAbsolutePath f) :content "deep"})
    (is (.exists f))
    (is (= "deep" (slurp f)))))

(deftest write-returns-byte-count-test
  (let [f      (tmp-file "bytes.txt")
        result (call sut/execute-write {:path (.getAbsolutePath f) :content "hello"})]
    (is (str/includes? (text-content result) "5 bytes"))))

;; ─── edit tool ───────────────────────────────────────────────────────────────

(deftest edit-exact-match-test
  (let [f (write-tmp "hello world\n")]
    (call sut/execute-edit {:path    (.getAbsolutePath f)
                            :oldText "hello"
                            :newText "goodbye"})
    (is (= "goodbye world\n" (slurp f)))))

(deftest edit-multiline-test
  (let [f (write-tmp "line1\nline2\nline3\n")]
    (call sut/execute-edit {:path    (.getAbsolutePath f)
                            :oldText "line1\nline2"
                            :newText "replaced"})
    (is (= "replaced\nline3\n" (slurp f)))))

(deftest edit-fuzzy-match-trailing-whitespace-test
  (testing "trailing whitespace on lines does not prevent match"
    (let [f (write-tmp "hello   \nworld")]
      (call sut/execute-edit {:path    (.getAbsolutePath f)
                              :oldText "hello\nworld"
                              :newText "bye"})
      (is (= "bye" (slurp f))))))

(deftest edit-file-not-found-test
  (is (thrown-with-msg? Exception #"File not found"
                        (call sut/execute-edit {:path    "/no/such/file.txt"
                                                :oldText "x"
                                                :newText "y"}))))

(deftest edit-text-not-found-test
  (let [f (write-tmp "hello world")]
    (is (thrown-with-msg? Exception #"Could not find"
                          (call sut/execute-edit {:path    (.getAbsolutePath f)
                                                  :oldText "goodbye"
                                                  :newText "hi"})))))

(deftest edit-ambiguous-match-test
  (let [f (write-tmp "foo bar foo")]
    (is (thrown-with-msg? Exception #"occurrences"
                          (call sut/execute-edit {:path    (.getAbsolutePath f)
                                                  :oldText "foo"
                                                  :newText "baz"})))))

(deftest edit-no-change-test
  (let [f (write-tmp "hello")]
    (is (thrown-with-msg? Exception #"identical"
                          (call sut/execute-edit {:path    (.getAbsolutePath f)
                                                  :oldText "hello"
                                                  :newText "hello"})))))

(deftest edit-preserves-crlf-line-endings-test
  (let [f   (write-tmp "line1\r\nline2\r\n")
        _   (call sut/execute-edit {:path    (.getAbsolutePath f)
                                    :oldText "line1"
                                    :newText "replaced"})
        out (slurp f)]
    (is (str/includes? out "\r\n"))
    (is (str/includes? out "replaced"))))

;; ─── bash tool ───────────────────────────────────────────────────────────────

(deftest bash-simple-command-test
  (let [result (call sut/execute-bash {:command "echo hello"})]
    (is (str/includes? (text-content result) "hello"))))

(deftest bash-stderr-merged-test
  (let [result (call sut/execute-bash {:command "echo out && echo err >&2"})
        out    (text-content result)]
    (is (str/includes? out "out"))
    (is (str/includes? out "err"))))

(deftest bash-nonzero-exit-throws-test
  (is (thrown-with-msg? Exception #"exited with code 1"
                        (call sut/execute-bash {:command "exit 1"}))))

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

(deftest bash-empty-output-test
  (let [result (call sut/execute-bash {:command "true"})]
    (is (= "(no output)" (text-content result)))))
