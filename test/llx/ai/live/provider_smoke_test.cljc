(ns ^{:kaocha/parallelize? true} llx.ai.live.provider-smoke-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]
             [llx.ai.test-util :as util]]
       :cljs [[cljs.test :refer [deftest is]]
              [llx.ai.test-util :as util :include-macros true]])
   [llx.ai :as client]
   [llx.ai.live.models :as models]
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
  (-> (util/collect-stream* stream 60000)
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

(deftest ^:llx/anthropic live-anthropic-smoke
  (util/async done
              (util/run-live-async!
               (run-provider-smoke* {:model           models/anthropic
                                     :opts            {:max-output-tokens 128}
                                     :complete-prompt "reply with exactly: llx anthropic smoke ok"
                                     :stream-prompt   "reply with exactly: llx anthropic stream ok"})
               done)))

(deftest ^:llx/google live-google-smoke
  (util/async done
              (util/run-live-async!
               (run-provider-smoke* {:model           models/google
                                     :opts            {:max-output-tokens 96
                                                       :reasoning         {:level :high :effort :high}}
                                     :complete-prompt "reply with exactly: llx google smoke ok"
                                     :stream-prompt   "reply with exactly: llx google stream ok"})
               done)))

(deftest ^:llx/mistral live-mistral-smoke
  (util/async done
              (util/run-live-async!
               (run-provider-smoke* {:model           models/mistral
                                     :opts            {:max-output-tokens 96}
                                     :complete-prompt "reply with exactly: llx mistral smoke ok"
                                     :stream-prompt   "reply with exactly: llx mistral stream ok"})
               done)))

(deftest ^:llx/ollama live-ollama-smoke
  (util/async done
              (util/run-live-async!
               (run-provider-smoke* {:model           models/ollama
                                     :opts            {:max-output-tokens 64}
                                     :complete-prompt "reply with exactly: llx ollama smoke ok"
                                     :stream-prompt   "reply with exactly: llx ollama stream ok"})
               done)))

(deftest ^:llx/openai live-openai-smoke
  (util/async done
              (util/run-live-async!
               (p/let [out (client/complete* env models/openai-completions
                                             {:messages [{:role      :user
                                                          :content   "reply with exactly: llx smoke ok"
                                                          :timestamp 1}]}
                                             {:max-output-tokens 64})]
                 (assert-complete-ok out)
                 true)
               done)))

(deftest ^:llx/openai live-openai-responses-smoke
  (util/async done
              (util/run-live-async!
               (run-provider-smoke* {:model           models/openai-responses
                                     :opts            {:max-output-tokens 96
                                                       :reasoning         {:effort :high :summary :detailed}}
                                     :complete-prompt "reply with exactly: llx openai responses smoke ok"
                                     :stream-prompt   "reply with exactly: llx openai responses stream ok"})
               done)))
