(ns llx-ai.demo
  (:require
   [babashka.json :as json]
   [llx-ai.client :as client]
   [llx-ai.client.jvm :as jvm]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.schema :as schema]))

(def demo-model
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
  (assoc demo-model
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

(def demo-context
  {:messages [{:role :user :content "say hello from llx" :timestamp 1}]})

(comment
  (schema/valid? :llx/model demo-model)
  (schema/valid? :llx/message (first (:messages demo-context)))
  (schema/valid? :llx/request-options {:max-output-tokens 64 :temperature 0.2})
  (jvm/complete demo-model
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
  (when-let [api-key (System/getenv "ANTHROPIC_API_KEY")]
    (let [stream (jvm/stream anthropic-model
                             {:messages [{:role :user :content "reply with exactly: llx anthropic stream demo ok" :timestamp 1}]}
                             {:api-key api-key :max-output-tokens 128})]
      (event-stream/drain! stream)
      (event-stream/result stream)))
  ;;
  )
