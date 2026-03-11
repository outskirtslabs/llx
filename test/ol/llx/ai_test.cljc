(ns ol.llx.ai-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]]
       :cljs [[cljs.test :refer-macros [deftest is]]])
   #?@(:clj [[ol.llx.ai.impl.client.jvm :as impl.jvm]]
       :cljs [[ol.llx.ai.impl.client.node :as impl.node]])
   [ol.llx.ai :as ai]
   [ol.llx.ai.impl.client :as impl.client]
   [ol.llx.ai.impl.schema :as impl.schema]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

#?(:clj (set! *warn-on-reflection* true))

(deftest default-env-delegates-to-jvm-provider
  (let [sentinel {:env :default}]
    #?(:clj
       (with-redefs [impl.jvm/default-env (fn [] sentinel)]
         (is (identical? sentinel (ai/default-env))))
       :cljs
       (with-redefs [impl.node/default-env (fn [] sentinel)]
         (is (identical? sentinel (ai/default-env)))))))

(deftest api-functions-delegate-with-explicit-env
  (let [env           {:env :explicit}
        model         {:id "m"}
        context       {:messages []}
        provider-opts {:max-output-tokens 8}
        unified-opts  {:max-tokens 8}
        assistant*    (p/resolved {:role :assistant})
        stream-handle (sp/chan)
        seen*         (atom [])]
    (with-redefs [impl.client/complete* (fn [env' model' context' opts']
                                          (swap! seen* conj [:complete* env' model' context' opts'])
                                          assistant*)
                  impl.client/stream*   (fn [env' model' context' opts']
                                          (swap! seen* conj [:stream* env' model' context' opts'])
                                          stream-handle)
                  impl.client/complete  (fn [env' model' context' opts']
                                          (swap! seen* conj [:complete env' model' context' opts'])
                                          assistant*)
                  impl.client/stream    (fn [env' model' context' opts']
                                          (swap! seen* conj [:stream env' model' context' opts'])
                                          stream-handle)]
      (is (p/deferred? (ai/complete* env model context provider-opts)))
      (is (sp/chan? (ai/stream* env model context provider-opts)))
      (is (p/deferred? (ai/complete env model context unified-opts)))
      (is (sp/chan? (ai/stream env model context unified-opts)))
      (is (= [[:complete* env model context provider-opts]
              [:stream* env model context provider-opts]
              [:complete env model context unified-opts]
              [:stream env model context unified-opts]]
             @seen*)))))

(deftest schema-regsitry-delegates-to-impl-registry
  (let [sentinel {:schema :registry}]
    (with-redefs [impl.schema/registry (fn [] sentinel)]
      (is (identical? sentinel (ai/schema-regsitry))))))
