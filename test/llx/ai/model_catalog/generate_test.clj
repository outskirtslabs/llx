(ns llx.ai.model-catalog.generate-test
  (:require
   [babashka.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [llx.ai.impl.model-catalog.generate :as generate]
   [llx.ai.impl.schema :as schema]))

(defn- resolve-public [sym]
  (try
    (requiring-resolve sym)
    (catch Throwable _
      nil)))

(defn- fixture-path [name]
  (str "test/llx/ai/fixtures/model_catalog/" name))

(defn- load-models-dev-fixture []
  (json/read-str (slurp (fixture-path "models_dev_sample.json"))
                 {:key-fn keyword}))

(defn- load-expected-fixture []
  (edn/read-string (slurp (fixture-path "expected_models_generated.edn"))))

(defn- add-openai-codex-source-fixture
  [fixture]
  (-> fixture
      (assoc-in [:openai :models "gpt-5.1-codex"]
                {:name       "GPT-5.1 Codex"
                 :tool_call  true
                 :reasoning  true
                 :limit      {:context 272000 :output 128000}
                 :cost       {:input 1.25 :output 10.0 :cache_read 0.125 :cache_write 1.25}
                 :modalities {:input ["text" "image"]}})
      (assoc-in [:openai :models "gpt-5-chat-latest"]
                {:name       "GPT-5 Chat Latest"
                 :tool_call  true
                 :reasoning  false
                 :limit      {:context 128000 :output 16384}
                 :cost       {:input 1.25 :output 10.0 :cache_read 0.125 :cache_write 0.0}
                 :modalities {:input ["text" "image"]}})
      (assoc-in [:anthropic :models "claude-opus-4-5"]
                {:name       "Claude Opus 4.5"
                 :tool_call  true
                 :reasoning  true
                 :limit      {:context 200000 :output 32000}
                 :cost       {:input 15.0 :output 75.0 :cache_read 0.0 :cache_write 0.0}
                 :modalities {:input ["text" "image"]}})))

(def overrides
  {"gpt-5-mini"
   {:name       "GPT-5 Mini Override"
    :max-tokens 60000}
   "gpt-4o-mini"
   {:name           "GPT-4o Mini Override"
    :provider       :openai
    :api            :openai-completions
    :base-url       "https://api.openai.com/v1"
    :context-window 128000
    :max-tokens     16384
    :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
    :capabilities   {:reasoning? false :input #{:text :image}}}})

(def provider-qualified-overrides
  {[:openai "gpt-5.1-codex"]
   {:cost {:output 5.0 :cache-write 1.25}}
   [:openai-codex "gpt-5.1-codex"]
   {:name           "GPT-5.1 Codex"
    :provider       :openai-codex
    :api            :openai-codex-responses
    :base-url       "https://chatgpt.com/backend-api"
    :context-window 272000
    :max-tokens     128000
    :cost           {:input 1.25 :output 10.0 :cache-read 0.125 :cache-write 0.0}
    :capabilities   {:reasoning? true :input #{:text :image}}}})

(deftest build-catalog-normalizes-supported-providers-only
  (let [build-catalog (resolve-public 'llx.ai.impl.model-catalog.generate/build-catalog)]
    (is (ifn? build-catalog))
    (when (ifn? build-catalog)
      (let [catalog (build-catalog {:models-dev-data (load-models-dev-fixture)
                                    :overrides       overrides})
            by-key  (into {} (map (juxt (fn [m] [(:provider m) (:id m)]) identity) catalog))]
        (is (= (load-expected-fixture) catalog))
        (is (= :openai-responses (get-in by-key [[:openai "gpt-5-mini"] :api])))
        (is (= 0.25 (get-in by-key [[:openai "gpt-5-mini"] :cost :input])))
        (is (= :anthropic-messages (get-in by-key [[:anthropic "claude-sonnet-4-5"] :api])))
        (is (= :google-generative-ai (get-in by-key [[:google "gemini-2.5-flash"] :api])))
        (is (= :openai-completions (get-in by-key [[:mistral "devstral-medium-latest"] :api])))
        (is (nil? (get by-key [:openai-compatible "llama3.2"])))
        (is (nil? (get by-key [:unknown-provider "mystery-1"])))
        (is (nil? (some #(when (= "mistral-no-tools" (:id %)) %) catalog)))
        (doseq [model catalog]
          (is (schema/valid? :llx/model model)))))))

(deftest build-catalog-is-deterministically-sorted
  (let [build-catalog (resolve-public 'llx.ai.impl.model-catalog.generate/build-catalog)]
    (is (ifn? build-catalog))
    (when (ifn? build-catalog)
      (let [input     (load-models-dev-fixture)
            expected  [[:anthropic "claude-sonnet-4-5"]
                       [:google "gemini-2.5-flash"]
                       [:mistral "devstral-medium-latest"]
                       [:openai "gpt-4o-mini"]
                       [:openai "gpt-5-mini"]]
            catalog-a (build-catalog {:models-dev-data input
                                      :overrides       overrides})
            catalog-b (build-catalog {:models-dev-data input
                                      :overrides       (into (sorted-map) overrides)})
            keys-a    (mapv (juxt :provider :id) catalog-a)
            keys-b    (mapv (juxt :provider :id) catalog-b)]
        (is (= expected keys-a))
        (is (= expected keys-b))
        (is (= catalog-a catalog-b))))))

(deftest build-catalog-rejects-invalid-override-shapes
  (let [build-catalog (resolve-public 'llx.ai.impl.model-catalog.generate/build-catalog)]
    (is (ifn? build-catalog))
    (when (ifn? build-catalog)
      (let [invalid-overrides {"gpt-5-mini" [:not-a-map]}]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"patch must be a map"
             (build-catalog {:models-dev-data (load-models-dev-fixture)
                             :overrides       invalid-overrides})))))))

(deftest build-catalog-rejects-duplicate-normalized-source-keys
  (let [build-catalog (resolve-public 'llx.ai.impl.model-catalog.generate/build-catalog)
        fixture       (load-models-dev-fixture)
        duplicate     {:name       "Duplicate Model"
                       :tool_call  true
                       :reasoning  false
                       :limit      {:context 4096 :output 1024}
                       :cost       {:input 0.0 :output 0.0 :cache_read 0.0 :cache_write 0.0}
                       :modalities {:input ["text"]}}
        fixture       (-> fixture
                          (assoc-in [:openai :models "duplicate-id"] duplicate)
                          (assoc-in [:openai :models :duplicate-id] duplicate))]
    (is (ifn? build-catalog))
    (when (ifn? build-catalog)
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Duplicate model key"
           (build-catalog {:models-dev-data fixture
                           :overrides       {}}))))))

(deftest build-catalog-rejects-provider-change-for-existing-id
  (let [build-catalog (resolve-public 'llx.ai.impl.model-catalog.generate/build-catalog)]
    (is (ifn? build-catalog))
    (when (ifn? build-catalog)
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot change provider"
           (build-catalog {:models-dev-data (load-models-dev-fixture)
                           :overrides       {"gpt-5-mini" {:provider :google}}}))))))

(deftest render-generated-source-is-stable
  (let [build-catalog (resolve-public 'llx.ai.impl.model-catalog.generate/build-catalog)
        render-source (resolve-public 'llx.ai.impl.model-catalog.generate/render-generated-source)]
    (is (ifn? build-catalog))
    (is (ifn? render-source))
    (when (and (ifn? build-catalog)
               (ifn? render-source))
      (let [catalog  (build-catalog {:models-dev-data (load-models-dev-fixture)
                                     :overrides       overrides})
            rendered (render-source catalog)]
        (is (string? rendered))
        (is (str/includes? rendered "This file is generated"))
        (is (str/includes? rendered "(ns llx.ai.impl.models-generated"))))))

(deftest build-catalog-deep-merges-nested-override-fields
  (let [build-catalog (resolve-public 'llx.ai.impl.model-catalog.generate/build-catalog)]
    (is (ifn? build-catalog))
    (when (ifn? build-catalog)
      (let [catalog (build-catalog
                     {:models-dev-data (load-models-dev-fixture)
                      :overrides       {"devstral-medium-latest"
                                        {:cost         {:cache-write 7.0}
                                         :capabilities {:reasoning? true}
                                         :compat       {:token-field    :max_tokens
                                                        :tool-id-format :mistral-9-alnum}}}})
            model   (first (filter #(= "devstral-medium-latest" (:id %)) catalog))]
        (is (= {:id             "devstral-medium-latest"
                :name           "Devstral Medium Source"
                :provider       :mistral
                :api            :openai-completions
                :base-url       "https://api.mistral.ai/v1"
                :context-window 128000
                :max-tokens     8192
                :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 7.0}
                :capabilities   {:reasoning? true :input #{:text}}
                :compat         {:token-field :max_tokens :tool-id-format :mistral-9-alnum}}
               model))))))

(deftest build-catalog-supports-provider-qualified-override-keys
  (let [build-catalog (resolve-public 'llx.ai.impl.model-catalog.generate/build-catalog)]
    (is (ifn? build-catalog))
    (when (ifn? build-catalog)
      (let [fixture (add-openai-codex-source-fixture (load-models-dev-fixture))
            catalog (build-catalog
                     {:models-dev-data fixture
                      :overrides       provider-qualified-overrides})
            by-key  (into {} (map (juxt (fn [m] [(:provider m) (:id m)]) identity) catalog))]
        (is (= 5.0 (get-in by-key [[:openai "gpt-5.1-codex"] :cost :output])))
        (is (= :openai-codex-responses (get-in by-key [[:openai-codex "gpt-5.1-codex"] :api])))))))

(deftest generate-models-uses-checked-in-overrides-and-emits-openai-codex-models
  (let [generate-models! (resolve-public 'llx.ai.impl.model-catalog.generate/generate-models!)
        rendered*        (atom nil)]
    (is (ifn? generate-models!))
    (when (ifn? generate-models!)
      (with-redefs [generate/fetch-models-dev-data
                    (fn [] (add-openai-codex-source-fixture (load-models-dev-fixture)))
                    generate/write-generated-artifact!
                    (fn [{:keys [content]}]
                      (reset! rendered* content)
                      :ok)]
        (is (= :ok (generate-models! {})))
        (is (string? @rendered*))
        (is (str/includes? @rendered* ":openai-codex"))
        (is (str/includes? @rendered* "\"gpt-5.2-codex\""))))))
