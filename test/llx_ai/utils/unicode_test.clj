(ns llx-ai.utils.unicode-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.utils.unicode :as sut]))

(set! *warn-on-reflection* true)

(defn- string-from-code-units
  [units]
  (String. (char-array (map char units))))

(deftest sanitize-surrogates-handles-surrogate-edge-cases
  (testing "preserves valid emoji surrogate pairs"
    (let [monkey (string-from-code-units [0xD83D 0xDE48])
          thumbs (string-from-code-units [0xD83D 0xDC4D])
          rocket (string-from-code-units [0xD83D 0xDE80])
          think  (string-from-code-units [0xD83E 0xDD14])
          input  (str monkey " " thumbs " " rocket " " think)]
      (is (= input (sut/sanitize-surrogates input)))))

  (testing "removes unpaired high surrogate"
    (let [high (string-from-code-units [0xD83D])]
      (is (= "Text  here" (sut/sanitize-surrogates (str "Text " high " here"))))))

  (testing "removes unpaired low surrogate"
    (let [low (string-from-code-units [0xDE48])]
      (is (= "Text  here" (sut/sanitize-surrogates (str "Text " low " here"))))))

  (testing "removes high surrogate at end of string"
    (let [high (string-from-code-units [0xD83D])]
      (is (= "trailing" (sut/sanitize-surrogates (str "trailing" high))))))

  (testing "removes multiple unpaired surrogates in one string"
    (let [high (string-from-code-units [0xD83D])
          low  (string-from-code-units [0xDE48])]
      (is (= "a b c" (sut/sanitize-surrogates (str "a" high " b" low " c"))))))

  (testing "preserves mixed content: emoji, CJK, math symbols, special quotes"
    (let [monkey (string-from-code-units [0xD83D 0xDE48])
          input  (str "emoji:" monkey
                      " Japanese:\u3053\u3093\u306B\u3061\u306F"
                      " Chinese:\u4F60\u597D"
                      " math:\u2211\u222B\u2202\u221A"
                      " quotes:\u201Ccurly\u201D \u2018quotes\u2019")]
      (is (= input (sut/sanitize-surrogates input)))))

  (testing "handles nil and empty input"
    (is (= "" (sut/sanitize-surrogates nil)))
    (is (= "" (sut/sanitize-surrogates ""))))

  (testing "passes through plain ASCII unchanged"
    (is (= "hello world 123" (sut/sanitize-surrogates "hello world 123")))))

(deftest sanitize-payload-deep-walks-data-structures
  (testing "sanitizes strings nested in maps, vectors, and mixed structures"
    (let [high    (string-from-code-units [0xD83D])
          monkey  (string-from-code-units [0xD83D 0xDE48])
          dirty   (str "dirty" high)
          clean   "dirty"
          payload {:system   (str "prompt" high)
                   :messages [{:role    "user"
                               :content [{:type "text"
                                          :text (str "hello " monkey " world" high)}]}
                              {:role    "tool-result"
                               :content [{:type "text"
                                          :text dirty}]}]
                   :number   42
                   :flag     true
                   :nothing  nil}]
      (is (= {:system   "prompt"
              :messages [{:role    "user"
                          :content [{:type "text"
                                     :text (str "hello " monkey " world")}]}
                         {:role    "tool-result"
                          :content [{:type "text"
                                     :text clean}]}]
              :number   42
              :flag     true
              :nothing  nil}
             (sut/sanitize-payload payload)))))

  (testing "real-world LinkedIn data with emoji preserved"
    (let [monkey  (string-from-code-units [0xD83D 0xDE48])
          content (str "Mario Zechner wann? Wo? Bin grad \u00E4u\u00DFersr eventuninformiert " monkey)
          payload {:messages [{:text content}]}
          result  (sut/sanitize-payload payload)]
      (is (= content (get-in result [:messages 0 :text])))))

  (testing "unpaired surrogates stripped from tool result content"
    (let [high    (string-from-code-units [0xD83D])
          payload {:tool-results [{:output (str "Text with unpaired surrogate: " high " <- should be sanitized")}]}
          result  (sut/sanitize-payload payload)]
      (is (= "Text with unpaired surrogate:  <- should be sanitized"
             (get-in result [:tool-results 0 :output])))))

  (testing "handles bare string input"
    (let [high (string-from-code-units [0xD83D])]
      (is (= "bare" (sut/sanitize-payload (str "bare" high))))))

  (testing "handles bare vector input"
    (let [high (string-from-code-units [0xD83D])]
      (is (= ["a" "b"] (sut/sanitize-payload [(str "a" high) "b"])))))

  (testing "passes through non-string leaves unchanged"
    (is (= {:a 1 :b [2 3] :c true :d nil}
           (sut/sanitize-payload {:a 1 :b [2 3] :c true :d nil})))))
