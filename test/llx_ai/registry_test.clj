(ns llx-ai.registry-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [llx-ai.registry :as registry]))

(def valid-adapter
  {:api             :openai-responses
   :build-request   (fn [_env _model _context _opts _stream?])
   :open-stream     (fn [_env _model _request-map])
   :decode-event    (fn [_env state _raw-chunk] {:state state :events []})
   :finalize        (fn [_env _state] {:assistant-message {} :events []})
   :normalize-error (fn [_env _ex _partial] {})})

(defn- with-registry-snapshot
  [f]
  (let [snapshot (registry/get-adapters)]
    (try
      (f)
      (finally
        (registry/clear!)
        (doseq [adapter snapshot]
          (registry/register! adapter))))))

(use-fixtures :each with-registry-snapshot)

(deftest register-and-get-adapter
  (registry/clear!)
  (registry/register! valid-adapter "test-source")
  (is (= :openai-responses (-> (registry/get-adapter :openai-responses) :api))))

(deftest unregister-source-removes-only-matching-entries
  (registry/clear!)
  (registry/register! valid-adapter "test-source")
  (registry/register! (assoc valid-adapter :api :google-generative-ai) "other-source")
  (registry/unregister-source! "test-source")
  (is (nil? (registry/get-adapter :openai-responses)))
  (is (= :google-generative-ai (-> (registry/get-adapter :google-generative-ai) :api))))

(deftest clear-removes-all-adapters
  (registry/clear!)
  (registry/register! valid-adapter "test-source")
  (registry/clear!)
  (is (empty? (registry/get-adapters))))
