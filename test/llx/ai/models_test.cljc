(ns llx.ai.models-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [llx.ai.impl.models :as models]
   [llx.ai.impl.schema :as schema]))

#?(:clj (set! *warn-on-reflection* true))

(deftest model-registry-and-lookup
  (let [providers (vec (models/get-providers))]
    (is (seq providers))
    (doseq [provider providers]
      (let [provider-models (vec (models/get-models provider))]
        (is (vector? provider-models))
        (doseq [model provider-models]
          (is (= provider (:provider model)))
          (is (schema/valid? :llx/model model))
          (is (= model (models/get-model provider (:id model)))))))
    (is (nil? (models/get-model :openai "__definitely_missing__")))
    (is (nil? (models/get-model :openai-compatible "llama3.2")))))

(deftest model-utility-functions
  (testing "calculate-cost"
    (let [cost (models/calculate-cost
                {:cost {:input 0.5 :output 1.5 :cache-read 0.1 :cache-write 0.25}}
                {:input 4000 :output 2000 :cache-read 2000 :cache-write 3000})]
      (is (= 0.002 (:input cost)))
      (is (= 0.003 (:output cost)))
      (is (= 0.0002 (:cache-read cost)))
      (is (= 0.00075 (:cache-write cost)))
      (is (< (#?(:clj Math/abs :cljs js/Math.abs)
              (- 0.00595 (:total cost)))
             1.0e-12))))
  (testing "supports-xhigh?"
    (is (true? (models/supports-xhigh? {:id  "claude-opus-4.6"
                                        :api :anthropic-messages})))
    (is (false? (models/supports-xhigh? {:id  "claude-opus-4.6"
                                         :api :openai-completions})))
    (is (false? (models/supports-xhigh? {:id  "gpt-4o-mini"
                                         :api :openai-completions}))))
  (testing "models-equal?"
    (is (true? (models/models-equal? {:provider :openai :id "gpt-4o-mini"}
                                     {:provider :openai :id "gpt-4o-mini"})))
    (is (false? (models/models-equal? {:provider :openai :id "gpt-4o-mini"}
                                      {:provider :openai-compatible :id "gpt-4o-mini"})))
    (is (false? (models/models-equal? {:provider :openai :id "gpt-4o-mini"} nil)))
    (is (false? (models/models-equal? nil {:provider :openai :id "gpt-4o-mini"})))))
