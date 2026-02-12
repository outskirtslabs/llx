(ns ^{:kaocha/parallelize? true} llx.ai.live.provider-smoke-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]
             [llx.ai.test-util :as util]]
       :cljs [[cljs.test :refer [deftest is async]]])
   [llx.ai :as client]
   [llx.ai.live.env :as live-env]
   [llx.ai.live.models :as models]
   [llx.ai.live.support :as support]
   [promesa.core :as p]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private env
  (client/default-env))

(defn- assert-complete-ok
  [out]
  (is (= :assistant (:role out)))
  (is (#{:stop :length :tool-use} (:stop-reason out)))
  (is (seq (:content out))))

(defn- assert-stream-ok*
  [stream]
  (-> (support/collect-stream* stream 60000)
      (p/then (fn [{:keys [events result]}]
                (is (= :start (:type (first events))))
                (is (#{:done :error} (:type (last events))))
                (is (= :assistant (:role result)))
                (is (seq (:content result)))))))

(defn- run-provider-smoke*
  [{:keys [model opts complete-prompt stream-prompt]}]
  (p/let [complete-out (client/complete* env model
                                         {:messages [{:role      :user
                                                      :content   complete-prompt
                                                      :timestamp 1}]}
                                         opts)
          _            (assert-complete-ok complete-out)
          _            (assert-stream-ok*
                        (client/stream* env model
                                        {:messages [{:role      :user
                                                     :content   stream-prompt
                                                     :timestamp 1}]}
                                        opts))]
    true))

(defn- run-live-smoke-test!
  [deferred]
  #?(:clj
     (do
       (util/await! deferred)
       true)
     :cljs
     deferred))

(deftest ^:llx/anthropic live-anthropic-smoke
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")]
    #?(:clj
       (run-live-smoke-test!
        (run-provider-smoke* {:model           models/anthropic
                              :opts            {:api-key api-key :max-output-tokens 128}
                              :complete-prompt "reply with exactly: llx anthropic smoke ok"
                              :stream-prompt   "reply with exactly: llx anthropic stream ok"}))
       :cljs
       (async done
              (-> (run-live-smoke-test!
                   (run-provider-smoke* {:model           models/anthropic
                                         :opts            {:api-key api-key :max-output-tokens 128}
                                         :complete-prompt "reply with exactly: llx anthropic smoke ok"
                                         :stream-prompt   "reply with exactly: llx anthropic stream ok"}))
                  (p/then (fn [_] (done)))
                  (p/catch (partial support/fail-and-done! done)))))))

(deftest ^:llx/google live-google-smoke
  (let [api-key (live-env/get-env "GEMINI_API_KEY")]
    #?(:clj
       (run-live-smoke-test!
        (run-provider-smoke* {:model           models/google
                              :opts            {:api-key           api-key
                                                :max-output-tokens 96
                                                :reasoning         {:level :high :effort :high}}
                              :complete-prompt "reply with exactly: llx google smoke ok"
                              :stream-prompt   "reply with exactly: llx google stream ok"}))
       :cljs
       (async done
              (-> (run-live-smoke-test!
                   (run-provider-smoke* {:model           models/google
                                         :opts            {:api-key           api-key
                                                           :max-output-tokens 96
                                                           :reasoning         {:level :high :effort :high}}
                                         :complete-prompt "reply with exactly: llx google smoke ok"
                                         :stream-prompt   "reply with exactly: llx google stream ok"}))
                  (p/then (fn [_] (done)))
                  (p/catch (partial support/fail-and-done! done)))))))

(deftest ^:llx/mistral live-mistral-smoke
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")]
    #?(:clj
       (run-live-smoke-test!
        (run-provider-smoke* {:model           models/mistral
                              :opts            {:api-key api-key :max-output-tokens 96}
                              :complete-prompt "reply with exactly: llx mistral smoke ok"
                              :stream-prompt   "reply with exactly: llx mistral stream ok"}))
       :cljs
       (async done
              (-> (run-live-smoke-test!
                   (run-provider-smoke* {:model           models/mistral
                                         :opts            {:api-key api-key :max-output-tokens 96}
                                         :complete-prompt "reply with exactly: llx mistral smoke ok"
                                         :stream-prompt   "reply with exactly: llx mistral stream ok"}))
                  (p/then (fn [_] (done)))
                  (p/catch (partial support/fail-and-done! done)))))))

(deftest ^:llx/ollama live-ollama-smoke
  #?(:clj
     (run-live-smoke-test!
      (run-provider-smoke* {:model           models/ollama
                            :opts            {:max-output-tokens 64}
                            :complete-prompt "reply with exactly: llx ollama smoke ok"
                            :stream-prompt   "reply with exactly: llx ollama stream ok"}))
     :cljs
     (async done
            (-> (run-live-smoke-test!
                 (run-provider-smoke* {:model           models/ollama
                                       :opts            {:max-output-tokens 64}
                                       :complete-prompt "reply with exactly: llx ollama smoke ok"
                                       :stream-prompt   "reply with exactly: llx ollama stream ok"}))
                (p/then (fn [_] (done)))
                (p/catch (partial support/fail-and-done! done))))))

(deftest ^:llx/openai live-openai-smoke
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    #?(:clj
       (run-live-smoke-test!
        (p/let [out (client/complete* env models/openai-completions
                                      {:messages [{:role      :user
                                                   :content   "reply with exactly: llx smoke ok"
                                                   :timestamp 1}]}
                                      {:api-key api-key :max-output-tokens 64})]
          (assert-complete-ok out)
          true))
       :cljs
       (async done
              (-> (p/let [out (client/complete* env models/openai-completions
                                                {:messages [{:role      :user
                                                             :content   "reply with exactly: llx smoke ok"
                                                             :timestamp 1}]}
                                                {:api-key api-key :max-output-tokens 64})]
                    (assert-complete-ok out)
                    true)
                  (p/then (fn [_] (done)))
                  (p/catch (partial support/fail-and-done! done)))))))

(deftest ^:llx/openai live-openai-responses-smoke
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    #?(:clj
       (run-live-smoke-test!
        (run-provider-smoke* {:model           models/openai-responses
                              :opts            {:api-key           api-key
                                                :max-output-tokens 96
                                                :reasoning         {:effort :high :summary :detailed}}
                              :complete-prompt "reply with exactly: llx openai responses smoke ok"
                              :stream-prompt   "reply with exactly: llx openai responses stream ok"}))
       :cljs
       (async done
              (-> (run-live-smoke-test!
                   (run-provider-smoke* {:model           models/openai-responses
                                         :opts            {:api-key           api-key
                                                           :max-output-tokens 96
                                                           :reasoning         {:effort :high :summary :detailed}}
                                         :complete-prompt "reply with exactly: llx openai responses smoke ok"
                                         :stream-prompt   "reply with exactly: llx openai responses stream ok"}))
                  (p/then (fn [_] (done)))
                  (p/catch (partial support/fail-and-done! done)))))))
