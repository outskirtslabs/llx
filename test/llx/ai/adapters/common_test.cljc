(ns llx.ai.adapters.common-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [llx.ai.impl.adapters.common :as sut]))

(deftest trim-trailing-slash-removes-single-trailing-slash
  (is (= "https://api.openai.com/v1"
         (sut/trim-trailing-slash "https://api.openai.com/v1/")))
  (is (= "https://api.openai.com/v1//"
         (sut/trim-trailing-slash "https://api.openai.com/v1///")))
  (is (= "https://api.openai.com/v1"
         (sut/trim-trailing-slash "https://api.openai.com/v1")))
  (is (nil? (sut/trim-trailing-slash nil))))

(deftest parse-json-safe-returns-decoded-or-empty-map
  (let [env {:json/decode-safe (fn [s _opts]
                                 (when (= s "{\"ok\":true}")
                                   {:ok true}))}]
    (is (= {:ok true} (sut/parse-json-safe env "{\"ok\":true}")))
    (is (= {} (sut/parse-json-safe env "invalid-json"))))
  (is (= {} (sut/parse-json-safe {} "{\"ok\":true}"))))

(deftest parse-json-lenient-falls-back-to-decode
  (testing "uses decode-safe when it succeeds"
    (let [env {:json/decode-safe (fn [_ _] {:safe true})
               :json/decode      (fn [_ _] {:decode true})}]
      (is (= {:safe true} (sut/parse-json-lenient env "{}")))))
  (testing "falls back to decode when decode-safe returns nil"
    (let [env {:json/decode-safe (fn [_ _] nil)
               :json/decode      (fn [_ _] {:decode true})}]
      (is (= {:decode true} (sut/parse-json-lenient env "{}")))))
  (testing "uses decode directly when decode-safe is absent"
    (let [env {:json/decode (fn [_ _] {:decode true})}]
      (is (= {:decode true} (sut/parse-json-lenient env "{}"))))))

(deftest empty-usage-shape-is-canonical
  (is (= {:input        0
          :output       0
          :cache-read   0
          :cache-write  0
          :total-tokens 0
          :cost         {:input       0.0
                         :output      0.0
                         :cache-read  0.0
                         :cache-write 0.0
                         :total       0.0}}
         (sut/empty-usage))))
