(ns llx-ai.models-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.schema :as schema]))

(set! *warn-on-reflection* true)

(defn- resolve-public
  [sym]
  (try
    (requiring-resolve sym)
    (catch Throwable _
      nil)))

(deftest model-registry-and-lookup
  (let [get-model     (resolve-public 'llx-ai.models/get-model)
        get-models    (resolve-public 'llx-ai.models/get-models)
        get-providers (resolve-public 'llx-ai.models/get-providers)]
    (is (ifn? get-providers))
    (is (ifn? get-models))
    (is (ifn? get-model))
    (when (and (ifn? get-providers)
               (ifn? get-models)
               (ifn? get-model))
      (let [providers (vec (get-providers))]
        (is (seq providers))
        (doseq [provider providers]
          (let [models (vec (get-models provider))]
            (is (vector? models))
            (doseq [model models]
              (is (= provider (:provider model)))
              (is (schema/valid? :llx/model model))
              (is (= model (get-model provider (:id model)))))))
        (is (nil? (get-model :openai "__definitely_missing__")))))))

(deftest model-utility-functions
  (testing "calculate-cost"
    (let [calculate-cost (resolve-public 'llx-ai.models/calculate-cost)]
      (is (ifn? calculate-cost))
      (when (ifn? calculate-cost)
        (let [cost (calculate-cost
                    {:cost {:input 0.5 :output 1.5 :cache-read 0.1 :cache-write 0.25}}
                    {:input 4000 :output 2000 :cache-read 2000 :cache-write 3000})]
          (is (= 0.002 (:input cost)))
          (is (= 0.003 (:output cost)))
          (is (= 0.0002 (:cache-read cost)))
          (is (= 0.00075 (:cache-write cost)))
          (is (< (Math/abs (- 0.00595 (:total cost))) 1.0e-12))))))
  (testing "supports-xhigh?"
    (let [supports-xhigh? (resolve-public 'llx-ai.models/supports-xhigh?)]
      (is (ifn? supports-xhigh?))
      (when (ifn? supports-xhigh?)
        (is (true? (supports-xhigh? {:id  "claude-opus-4.6"
                                     :api :anthropic-messages})))
        (is (false? (supports-xhigh? {:id  "claude-opus-4.6"
                                      :api :openai-completions})))
        (is (false? (supports-xhigh? {:id  "gpt-4o-mini"
                                      :api :openai-completions}))))))
  (testing "models-equal?"
    (let [models-equal? (resolve-public 'llx-ai.models/models-equal?)]
      (is (ifn? models-equal?))
      (when (ifn? models-equal?)
        (is (true? (models-equal? {:provider :openai :id "gpt-4o-mini"}
                                  {:provider :openai :id "gpt-4o-mini"})))
        (is (false? (models-equal? {:provider :openai :id "gpt-4o-mini"}
                                   {:provider :openai-compatible :id "gpt-4o-mini"})))
        (is (false? (models-equal? {:provider :openai :id "gpt-4o-mini"} nil)))
        (is (false? (models-equal? nil {:provider :openai :id "gpt-4o-mini"})))))))
