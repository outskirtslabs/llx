(ns llx.ai.live.cross-provider-handoff-smoke-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]
             [llx.ai.test-util :as util]]
       :cljs [[cljs.test :refer [deftest is async]]
              [llx.ai.live.support :as support]])
   [llx.ai :as client]
   [llx.ai.live.env :as live-env]
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

(defn- run-live!
  [deferred]
  #?(:clj (do
            (util/await! deferred)
            true)
     :cljs deferred))

#?(:cljs
   (defn- run-async!
     [done deferred]
     (-> deferred
         (p/then (fn [_] (done)))
         (p/catch (partial support/fail-and-done! done)))))

(deftest ^{:llx/openai true :llx/anthropic true} live-handoff-openai-to-anthropic
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/openai-completions {:api-key openai-key :max-output-tokens 96}
         "Give one short sentence about Clojure."
         models/anthropic {:api-key anthropic-key :max-output-tokens 128}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/openai-completions {:api-key openai-key :max-output-tokens 96}
                 "Give one short sentence about Clojure."
                 models/anthropic {:api-key anthropic-key :max-output-tokens 128})))))))

(deftest ^{:llx/openai true :llx/anthropic true} live-handoff-anthropic-to-openai
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/anthropic {:api-key anthropic-key :max-output-tokens 96}
         "Give one short sentence about Lisp history."
         models/openai-completions {:api-key openai-key :max-output-tokens 128}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/anthropic {:api-key anthropic-key :max-output-tokens 96}
                 "Give one short sentence about Lisp history."
                 models/openai-completions {:api-key openai-key :max-output-tokens 128})))))))

(deftest ^{:llx/openai true :llx/anthropic true} live-handoff-openai-responses-to-anthropic
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/openai-responses {:api-key           openai-key
                                  :max-output-tokens 128
                                  :reasoning         {:effort :high :summary :detailed}}
         "Give one short sentence about Clojure macros."
         models/anthropic {:api-key anthropic-key :max-output-tokens 128}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/openai-responses {:api-key           openai-key
                                          :max-output-tokens 128
                                          :reasoning         {:effort :high :summary :detailed}}
                 "Give one short sentence about Clojure macros."
                 models/anthropic {:api-key anthropic-key :max-output-tokens 128})))))))

(deftest ^{:llx/openai true :llx/anthropic true} live-handoff-anthropic-to-openai-responses
  (let [openai-key    (live-env/get-env "OPENAI_API_KEY")
        anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/anthropic {:api-key anthropic-key :max-output-tokens 96}
         "Give one short sentence about Lisp history."
         models/openai-responses {:api-key           openai-key
                                  :max-output-tokens 128
                                  :reasoning         {:effort :high :summary :detailed}}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/anthropic {:api-key anthropic-key :max-output-tokens 96}
                 "Give one short sentence about Lisp history."
                 models/openai-responses {:api-key           openai-key
                                          :max-output-tokens 128
                                          :reasoning         {:effort :high :summary :detailed}})))))))

(deftest ^:llx/openai live-handoff-openai-responses-to-openai-completions
  (let [openai-key (live-env/get-env "OPENAI_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/openai-responses {:api-key           openai-key
                                  :max-output-tokens 128
                                  :reasoning         {:effort :high :summary :detailed}}
         "Give one short sentence about Clojure protocols."
         models/openai-completions {:api-key openai-key :max-output-tokens 128}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/openai-responses {:api-key           openai-key
                                          :max-output-tokens 128
                                          :reasoning         {:effort :high :summary :detailed}}
                 "Give one short sentence about Clojure protocols."
                 models/openai-completions {:api-key openai-key :max-output-tokens 128})))))))

(deftest ^{:llx/anthropic true :llx/google true} live-handoff-anthropic-to-google
  (let [anthropic-key (live-env/get-env "ANTHROPIC_API_KEY")
        google-key    (live-env/get-env "GEMINI_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/anthropic {:api-key anthropic-key :max-output-tokens 96}
         "Give one short sentence about Clojure macros."
         models/google {:api-key           google-key
                        :max-output-tokens 128
                        :reasoning         {:level :high :effort :high}}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/anthropic {:api-key anthropic-key :max-output-tokens 96}
                 "Give one short sentence about Clojure macros."
                 models/google {:api-key           google-key
                                :max-output-tokens 128
                                :reasoning         {:level :high :effort :high}})))))))

(deftest ^{:llx/google true :llx/openai true} live-handoff-google-to-openai-completions
  (let [openai-key (live-env/get-env "OPENAI_API_KEY")
        google-key (live-env/get-env "GEMINI_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/google {:api-key           google-key
                        :max-output-tokens 96
                        :reasoning         {:level :high :effort :high}}
         "Give one short sentence about Clojure protocols."
         models/openai-completions {:api-key openai-key :max-output-tokens 128}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/google {:api-key           google-key
                                :max-output-tokens 96
                                :reasoning         {:level :high :effort :high}}
                 "Give one short sentence about Clojure protocols."
                 models/openai-completions {:api-key openai-key :max-output-tokens 128})))))))

(deftest ^{:llx/google true :llx/mistral true} live-handoff-google-to-mistral
  (let [google-key  (live-env/get-env "GEMINI_API_KEY")
        mistral-key (live-env/get-env "MISTRAL_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/google {:api-key           google-key
                        :max-output-tokens 96
                        :reasoning         {:level :high :effort :high}}
         "Give one short sentence about Clojure protocols."
         models/mistral {:api-key mistral-key :max-output-tokens 128}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/google {:api-key           google-key
                                :max-output-tokens 96
                                :reasoning         {:level :high :effort :high}}
                 "Give one short sentence about Clojure protocols."
                 models/mistral {:api-key mistral-key :max-output-tokens 128})))))))

(deftest ^{:llx/mistral true :llx/ollama true} live-handoff-mistral-to-ollama
  (let [mistral-key (live-env/get-env "MISTRAL_API_KEY")]
    #?(:clj
       (run-live!
        (handoff-check!*
         models/mistral {:api-key mistral-key :max-output-tokens 96}
         "Give one short sentence about Clojure macros."
         models/ollama {:max-output-tokens 128}))
       :cljs
       (async done
              (run-async!
               done
               (run-live!
                (handoff-check!*
                 models/mistral {:api-key mistral-key :max-output-tokens 96}
                 "Give one short sentence about Clojure macros."
                 models/ollama {:max-output-tokens 128})))))))
