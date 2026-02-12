(ns llx.ai.live.cross-provider-handoff-smoke-test
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

(defn- handoff-check!*
  [model-a opts-a prompt-a model-b opts-b]
  (let [user1 {:role      :user
               :content   prompt-a
               :timestamp 1}
        user2 {:role      :user
               :content   "Now say hi and mention the previous sentence briefly."
               :timestamp 2}]
    (p/let [step-1 (client/complete* env model-a {:messages [user1]} opts-a)
            step-2 (client/complete* env model-b {:messages [user1 step-1 user2]} opts-b)]
      (is (not= :error (:stop-reason step-1)))
      (is (not= :error (:stop-reason step-2)))
      (is (seq (:content step-2)))
      (is (#{:stop :length :tool-use} (:stop-reason step-2)))
      true)))

(defn- run-live-async!
  [deferred done]
  (-> deferred
      (p/then (fn [_] (done)))
      (p/catch (partial util/fail-and-done! done))))

(deftest ^{:llx/openai true :llx/anthropic true} live-handoff-openai-to-anthropic
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/openai-completions {:max-output-tokens 96}
                "Give one short sentence about Clojure."
                models/anthropic {:max-output-tokens 128})
               done)))

(deftest ^{:llx/openai true :llx/anthropic true} live-handoff-anthropic-to-openai
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/anthropic {:max-output-tokens 96}
                "Give one short sentence about Lisp history."
                models/openai-completions {:max-output-tokens 128})
               done)))

(deftest ^{:llx/openai true :llx/anthropic true} live-handoff-openai-responses-to-anthropic
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/openai-responses {:max-output-tokens 128
                                         :reasoning         {:effort :high :summary :detailed}}
                "Give one short sentence about Clojure macros."
                models/anthropic {:max-output-tokens 128})
               done)))

(deftest ^{:llx/openai true :llx/anthropic true} live-handoff-anthropic-to-openai-responses
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/anthropic {:max-output-tokens 96}
                "Give one short sentence about Lisp history."
                models/openai-responses {:max-output-tokens 128
                                         :reasoning         {:effort :high :summary :detailed}})
               done)))

(deftest ^:llx/openai live-handoff-openai-responses-to-openai-completions
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/openai-responses {:max-output-tokens 128
                                         :reasoning         {:effort :high :summary :detailed}}
                "Give one short sentence about Clojure protocols."
                models/openai-completions {:max-output-tokens 128})
               done)))

(deftest ^{:llx/anthropic true :llx/google true} live-handoff-anthropic-to-google
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/anthropic {:max-output-tokens 96}
                "Give one short sentence about Clojure macros."
                models/google {:max-output-tokens 128
                               :reasoning         {:level :high :effort :high}})
               done)))

(deftest ^{:llx/google true :llx/openai true} live-handoff-google-to-openai-completions
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/google {:max-output-tokens 96
                               :reasoning         {:level :high :effort :high}}
                "Give one short sentence about Clojure protocols."
                models/openai-completions {:max-output-tokens 128})
               done)))

(deftest ^{:llx/google true :llx/mistral true} live-handoff-google-to-mistral
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/google {:max-output-tokens 96
                               :reasoning         {:level :high :effort :high}}
                "Give one short sentence about Clojure protocols."
                models/mistral {:max-output-tokens 128})
               done)))

(deftest ^{:llx/mistral true :llx/ollama true} live-handoff-mistral-to-ollama
  (util/async done
              (run-live-async!
               (handoff-check!*
                models/mistral {:max-output-tokens 96}
                "Give one short sentence about Clojure macros."
                models/ollama {:max-output-tokens 128})
               done)))
