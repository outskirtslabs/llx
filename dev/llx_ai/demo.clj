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

(def demo-context
  {:messages [{:role :user :content "say hello from llx" :timestamp 1}]})

(defn schema-check
  [])

(defn stub-complete
  []
  (let [seen-request (atom nil)
        env          {:http/request (fn [request]
                                      (reset! seen-request request)
                                      {:status 200
                                       :body   (json/write-str
                                                {:choices [{:finish_reason "stop"
                                                            :message       {:role    "assistant"
                                                                            :content "hello from stub"}}]
                                                 :usage   {:prompt_tokens     7
                                                           :completion_tokens 3
                                                           :total_tokens      10}})})
                      :json/encode  json/write-str
                      :json/decode  (fn [s _opts] (json/read-str s {:key-fn keyword}))
                      :clock/now-ms (fn [] 1730000000000)
                      :id/new       (fn [] "id-1")
                      :env/get      (fn [_k] nil)}
        response     (client/complete env demo-model demo-context {:api-key "demo-key" :max-output-tokens 64})]
    {:response response
     :request  @seen-request}))

(defn stub-stream
  []
  (let [env    {:http/request (fn [_request]
                                {:status 200
                                 :body   (java.io.ByteArrayInputStream.
                                          (.getBytes
                                           (str "data: " (json/write-str {:choices [{:delta {:content "hello "}}]}) "\n"
                                                "data: " (json/write-str {:choices [{:delta {:content "stream"} :finish_reason "stop"}]
                                                                          :usage   {:prompt_tokens     2
                                                                                    :completion_tokens 2
                                                                                    :total_tokens      4}}) "\n"
                                                "data: [DONE]\n")
                                           "UTF-8"))})
               :json/encode  json/write-str
               :json/decode  (fn [s _opts] (json/read-str s {:key-fn keyword}))
               :clock/now-ms (fn [] 1730000000000)
               :id/new       (fn [] "id-1")
               :env/get      (fn [_k] nil)}
        stream (client/stream env demo-model demo-context {:api-key "demo-key" :max-output-tokens 64})]
    {:events (event-stream/drain! stream)
     :result (event-stream/result stream)}))

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
  (let [stream (jvm/stream ollama-model
                           {:messages [{:role :user :content "reply with exactly: llx stream demo ok" :timestamp 1}]}
                           {:max-output-tokens 64 :temperature 0.0})]
    (event-stream/drain! stream)
    (event-stream/result stream))
  ;;
  )
