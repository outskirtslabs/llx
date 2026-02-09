(ns llx-ai.utils.unicode-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.utils.unicode :as sut]))

(defn- string-from-code-units
  [units]
  (String. (char-array (map char units))))

(deftest sanitize-surrogates
  (testing "preserves valid pairs"
    (let [monkey (string-from-code-units [0xD83D 0xDE48])
          input  (str "Hello " monkey " World")]
      (is (= input (sut/sanitize-surrogates input)))))
  (testing "removes unpaired high"
    (let [high-only (string-from-code-units [0xD83D])
          input     (str "Text " high-only " here")]
      (is (= "Text  here" (sut/sanitize-surrogates input)))))
  (testing "removes unpaired low"
    (let [low-only (string-from-code-units [0xDE48])
          input    (str "Text " low-only " here")]
      (is (= "Text  here" (sut/sanitize-surrogates input))))))
