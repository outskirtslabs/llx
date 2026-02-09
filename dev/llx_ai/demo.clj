(ns llx-ai.demo
  (:require
   [llx-ai.client.jvm :as jvm]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.schema :as schema]))

(set! *warn-on-reflection* true)

(def openai-model
  {:id             "gpt-5.2-2025-12-11"
   :name           "GPT-5.2 "
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def ollama-model
  (assoc openai-model
         :id "devstral-small-2:latest"
         :name "Ollama local"
         :provider :openai-compatible
         :base-url "http://localhost:11434/v1"))

(def anthropic-model
  {:id             "claude-sonnet-4-5"
   :name           "Claude Sonnet 4.5"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       "https://api.anthropic.com"
   :context-window 200000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(def google-model
  {:id             "gemini-2.5-flash"
  :name           "Gemini 2.5 Flash"
  :provider       :google
  :api            :google-generative-ai
   :base-url       "https://generativelanguage.googleapis.com/v1beta"
   :context-window 1048576
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(def mistral-model
  {:id             "devstral-medium-latest"
   :name           "Devstral Medium"
   :provider       :mistral
   :api            :openai-completions
   :base-url       "https://api.mistral.ai/v1"
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def demo-context
  {:messages [{:role :user :content "say hello from llx" :timestamp 1}]})

(comment
  (schema/valid? :llx/model openai-model)
  (schema/valid? :llx/message (first (:messages demo-context)))
  (schema/valid? :llx/request-options {:max-output-tokens 64 :temperature 0.2})
  (jvm/complete openai-model
                {:messages [{:role :user :content "Who are you?" :timestamp 1}]}
                {:max-output-tokens 64 :temperature 0.0})
  (jvm/complete ollama-model
                {:messages [{:role :user :content "Who are you?" :timestamp 1}]}
                {:max-output-tokens 64 :temperature 0.0})
  (jvm/complete anthropic-model
                {:messages [{:role :user :content "Who are you?" :timestamp 1}]}
                {:max-output-tokens 128})
  (let [stream (jvm/stream ollama-model
                           {:messages [{:role :user :content "reply with exactly: llx stream demo ok" :timestamp 1}]}
                           {:max-output-tokens 64 :temperature 0.0})]
    (event-stream/drain! stream)
    (event-stream/result stream))
  (let [stream (jvm/stream anthropic-model
                           {:messages [{:role :user :content "reply with exactly: llx anthropic stream demo ok" :timestamp 1}]}
                           {:max-output-tokens 128})]
    (event-stream/drain! stream)
    (event-stream/result stream))
  (jvm/complete google-model
                {:messages [{:role :user :content "reply with exactly: llx google demo ok" :timestamp 1}]}
                {:max-output-tokens 96
                 :reasoning         {:level :high :effort :high :summary :detailed}})
  (jvm/complete mistral-model
                {:messages [{:role :user :content "reply with exactly: llx mistral demo ok" :timestamp 1}]}
                {:max-output-tokens 96})
  (let [stream (jvm/stream google-model
                           {:messages [{:role :user :content "reply with exactly: llx google stream demo ok" :timestamp 1}]}
                           {:max-output-tokens 96
                            :reasoning         {:level :high :effort :high :summary :detailed}})]
    (event-stream/drain! stream)
    (event-stream/result stream))
  (let [stream (jvm/stream mistral-model
                           {:messages [{:role :user :content "reply with exactly: llx mistral stream demo ok" :timestamp 1}]}
                           {:max-output-tokens 96})]
    (event-stream/drain! stream)
    (event-stream/result stream))
  ;; Handoff: Anthropic -> Google
  (let [user1  {:role :user :content "Give one short sentence about Clojure macros." :timestamp 1}
        step-1 (jvm/complete anthropic-model
                             {:messages [user1]}
                             {:max-output-tokens 96})
        user2  {:role :user :content "Now say hi and summarize your prior sentence." :timestamp 2}]
    (jvm/complete google-model
                  {:messages [user1 step-1 user2]}
                  {:max-output-tokens 128
                   :reasoning         {:level :high :effort :high :summary :detailed}}))
  ;; Handoff: Google -> OpenAI Completions
  (let [user1  {:role :user :content "Give one short sentence about Clojure protocols." :timestamp 1}
        step-1 (jvm/complete google-model
                             {:messages [user1]}
                             {:max-output-tokens 96
                              :reasoning         {:level :high :effort :high :summary :detailed}})
        user2  {:role :user :content "Now say hi and summarize your prior sentence." :timestamp 2}]
    (jvm/complete openai-model
                  {:messages [user1 step-1 user2]}
                  {:max-output-tokens 96
                   :temperature       0.0}))
  ;; Handoff: Google -> Mistral
  (let [user1       {:role :user :content "Give one short sentence about Clojure protocols." :timestamp 1}
        step-1      (jvm/complete google-model
                                  {:messages [user1]}
                                  {:max-output-tokens 96
                                   :reasoning         {:level :high :effort :high :summary :detailed}})
        user2       {:role :user :content "Now say hi and summarize your prior sentence." :timestamp 2}]
    (jvm/complete mistral-model
                  {:messages [user1 step-1 user2]}
                  {:max-output-tokens 128}))
  ;; Handoff: Mistral -> OpenAI-compatible
  (let [user1       {:role :user :content "Give one short sentence about Clojure macros." :timestamp 1}
        step-1      (jvm/complete mistral-model
                                  {:messages [user1]}
                                  {:max-output-tokens 96})
        user2       {:role :user :content "Now say hi and summarize your prior sentence." :timestamp 2}]
    (jvm/complete ollama-model
                  {:messages [user1 step-1 user2]}
                  {:max-output-tokens 128
                   :temperature       0.0}))
  ;;

  (comment
    ;; OpenAI Responses demo model + request options
    (def openai-responses-model
      {:id             "gpt-5-mini"
       :name           "GPT-5 Mini"
       :provider       :openai
       :api            :openai-responses
       :base-url       "https://api.openai.com/v1"
       :context-window 400000
       :max-tokens     128000
       :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
       :capabilities   {:reasoning? true :input #{:text}}})

    ;; Responses complete
    (jvm/complete openai-responses-model
                  {:messages [{:role :user :content "reply with exactly: llx openai responses demo ok" :timestamp 1}]}
                  {:max-output-tokens 96
                   :reasoning         {:level :high :effort :high :summary :detailed}
                   :session-id        "demo-openai-responses"
                   :cache-control     :short})
;; ERROR
;; =>  Schema validation failed

    ;; Responses stream
    (let [stream (jvm/stream openai-responses-model
                             {:messages [{:role :user :content "reply with exactly: llx openai responses stream demo ok" :timestamp 1}]}
                             {:max-output-tokens 96
                              :reasoning         {:level :high :effort :high :summary :detailed}})]
      (event-stream/drain! stream)
      (event-stream/result stream))

    ;; Handoff: OpenAI Responses -> Anthropic
    (let [user1  {:role :user :content "Give one short sentence about Clojure protocols." :timestamp 1}
          step-1 (jvm/complete openai-responses-model
                               {:messages [user1]}
                               {:max-output-tokens 128
                                :reasoning         {:level :high :effort :high :summary :detailed}})
          user2  {:role :user :content "Now say hi and summarize your prior sentence." :timestamp 2}]
      (jvm/complete anthropic-model
                    {:messages [user1 step-1 user2]}
                    {:max-output-tokens 128}))

    ;; Handoff: Anthropic -> OpenAI Responses
    (let [user1  {:role :user :content "Give one short sentence about Lisp history." :timestamp 1}
          step-1 (jvm/complete anthropic-model
                               {:messages [user1]}
                               {:max-output-tokens 96})
          user2  {:role :user :content "Now say hi and summarize your prior sentence." :timestamp 2}]
      (jvm/complete openai-responses-model
                    {:messages [user1 step-1 user2]}
                    {:max-output-tokens 128
                     :reasoning         {:level :high :effort :high :summary :detailed}}))))
