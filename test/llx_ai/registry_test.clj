(ns llx-ai.registry-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.registry :as registry]))

(set! *warn-on-reflection* true)

(def valid-adapter
  {:api             :openai-responses
   :build-request   (fn [_env _model _context _opts _stream?])
   :open-stream     (fn [_env _model _request-map])
   :decode-event    (fn [_env state _raw-chunk] {:state state :events []})
   :finalize        (fn [_env _state] {:assistant-message {} :events []})
   :normalize-error (fn [_env _ex _partial] {})})

(deftest registry-patterns
  (testing "immutable registry returns new value"
    (let [registry (registry/immutable-registry)
          updated  (registry/register-adapter registry valid-adapter "source-a")]
      (is (nil? (registry/get-adapter registry :openai-responses)))
      (is (= :openai-responses (-> (registry/get-adapter updated :openai-responses) :api)))
      (is (contains? updated registry/adapters-key))
      (is (contains? updated registry/tools-key))))
  (testing "mutable registry"
    (let [registry* (atom (registry/immutable-registry))
          impl      (registry/mutable-registry registry*)]
      (swap! registry* registry/register-adapter valid-adapter "source-a")
      (is (= :openai-responses (-> (registry/get-adapter impl :openai-responses) :api)))))
  (testing "dynamic registry"
    (let [dynamic-impl (registry/dynamic-registry)
          local-reg    (registry/register-adapter (registry/immutable-registry) valid-adapter "source-a")]
      (binding [registry/*registry* local-reg]
        (is (= :openai-responses (-> (registry/get-adapter dynamic-impl :openai-responses) :api)))))))

(deftest registry-operations
  (testing "unregister and clear adapters"
    (let [registry (-> (registry/immutable-registry)
                       (registry/register-adapter valid-adapter "source-a")
                       (registry/register-adapter (assoc valid-adapter :api :google-generative-ai) "source-b"))
          removed  (registry/unregister-adapters-by-source registry "source-a")
          cleared  (registry/clear-adapters removed)]
      (is (nil? (registry/get-adapter removed :openai-responses)))
      (is (= :google-generative-ai (-> (registry/get-adapter removed :google-generative-ai) :api)))
      (is (empty? (registry/get-adapters cleared)))))
  (testing "rejects invalid api value"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Schema validation failed|Validation Error"
         (registry/get-adapter (registry/immutable-registry) "openai-responses")))))
