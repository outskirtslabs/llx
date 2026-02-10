(ns llx-ai.live.models
  "Canonical model definitions shared across all live test suites."
  (:require
   [llx-ai.live.env :as live-env]))

(set! *warn-on-reflection* true)

(def openai-responses
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_MODEL") "gpt-5-mini")
   :name           "OpenAI Responses"
   :provider       :openai
   :api            :openai-responses
   :base-url       (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_BASE_URL") "https://api.openai.com/v1")
   :context-window 400000
   :max-tokens     128000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(def openai-completions
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_MODEL") "gpt-4o-mini")
   :name           "OpenAI Completions"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text :image}}})

(def anthropic
  {:id             (or (live-env/get-env "LLX_LIVE_ANTHROPIC_MODEL") "claude-3-5-haiku-20241022")
   :name           "Anthropic"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       (or (live-env/get-env "LLX_LIVE_ANTHROPIC_BASE_URL") "https://api.anthropic.com")
   :context-window 200000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text :image}}})

(def google
  {:id             (or (live-env/get-env "LLX_LIVE_GOOGLE_MODEL") "gemini-2.5-flash")
   :name           "Google"
   :provider       :google
   :api            :google-generative-ai
   :base-url       (or (live-env/get-env "LLX_LIVE_GOOGLE_BASE_URL") "https://generativelanguage.googleapis.com/v1beta")
   :context-window 1048576
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(def mistral
  {:id             (or (live-env/get-env "LLX_LIVE_MISTRAL_MODEL") "devstral-medium-latest")
   :name           "Mistral"
   :provider       :mistral
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_MISTRAL_BASE_URL") "https://api.mistral.ai/v1")
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def mistral-pixtral
  {:id             (or (live-env/get-env "LLX_LIVE_MISTRAL_PIXTRAL_MODEL") "pixtral-12b")
   :name           "Mistral Pixtral"
   :provider       :mistral
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_MISTRAL_BASE_URL") "https://api.mistral.ai/v1")
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text :image}}})

(def ollama
  {:id             (or (live-env/get-env "LLX_LIVE_OLLAMA_MODEL") "gpt-oss:20b")
   :name           "Ollama GPT-OSS 20B"
   :provider       :openai-compatible
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_OLLAMA_BASE_URL") "http://localhost:11434/v1")
   :context-window 128000
   :max-tokens     16000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})
