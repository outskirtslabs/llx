(ns llx-ai.client-test
  (:require
   [babashka.json :as json]
   [clojure.test :refer [deftest is]]
   [llx-ai.client :as sut]))

(def base-model
  {:id             "gpt-4o-mini"
   :name           "GPT-4o Mini"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(defn stub-env
  [handler]
  {:http/request handler
   :json/encode  json/write-str
   :json/decode  (fn [s _opts] (json/read-str s {:key-fn keyword}))
   :clock/now-ms (fn [] 1730000000000)
   :id/new       (fn [] "id-1")})

(deftest complete-openai-completions-happy-path
  (let [seen-request (atom nil)
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (json/write-str
                                           {:choices [{:finish_reason "stop"
                                                       :message       {:role    "assistant"
                                                                       :content "hello from llx"}}]
                                            :usage   {:prompt_tokens     12
                                                      :completion_tokens 8
                                                      :total_tokens      20}})}))
        context      {:messages [{:role :user :content "say hello" :timestamp 1}]}
        opts         {:api-key           "test-openai-key"
                      :max-output-tokens 128
                      :temperature       0.2}
        out          (sut/complete env base-model context opts)
        request      @seen-request
        payload      (json/read-str (:body request) {:key-fn keyword})]
    (is
     (= {:role        :assistant
         :stop-reason :stop
         :content     [{:type :text :text "hello from llx"}]
         :api         :openai-completions
         :provider    :openai
         :model       "gpt-4o-mini"
         :timestamp   1730000000000
         :usage       {:input 12 :output 8 :cache-read 0 :cache-write 0 :total-tokens 20}}
        (-> out
            (select-keys [:role :stop-reason :content :api :provider :model :timestamp])
            (assoc :usage (select-keys (:usage out) [:input :output :cache-read :cache-write :total-tokens])))))
    (is
     (= {:method  :post
         :url     "https://api.openai.com/v1/chat/completions"
         :throw   false
         :auth    "Bearer test-openai-key"
         :ctype   "application/json"
         :payload {:model                 "gpt-4o-mini"
                   :messages              [{:role "user" :content "say hello"}]
                   :stream                false
                   :max_completion_tokens 128
                   :temperature           0.2}}
        {:method  (:method request)
         :url     (:url request)
         :throw   (:throw request)
         :auth    (get-in request [:headers "Authorization"])
         :ctype   (get-in request [:headers "Content-Type"])
         :payload (select-keys payload [:model :messages :stream :max_completion_tokens :temperature])}))))

(deftest complete-openai-completions-tool-call-response
  (let [env     (stub-env
                 (fn [_request]
                   {:status 200
                    :body   (json/write-str
                             {:choices [{:finish_reason "tool_calls"
                                         :message       {:role       "assistant"
                                                         :content    nil
                                                         :tool_calls [{:id       "call_1"
                                                                       :type     "function"
                                                                       :function {:name      "search"
                                                                                  :arguments "{\"q\":\"foo\"}"}}]}}]
                              :usage   {:prompt_tokens     40
                                        :completion_tokens 3
                                        :total_tokens      43}})}))
        context {:messages [{:role :user :content "run search" :timestamp 1}]}
        out     (sut/complete env base-model context {:api-key "x"})]
    (is
     (= {:stop-reason :tool-use
         :content     [{:type :tool-call :id "call_1" :name "search" :arguments {:q "foo"}}]
         :usage       {:input 40 :output 3 :cache-read 0 :cache-write 0 :total-tokens 43}}
        {:stop-reason (:stop-reason out)
         :content     (:content out)
         :usage       (select-keys (:usage out) [:input :output :cache-read :cache-write :total-tokens])}))))

(deftest complete-openai-completions-non-2xx
  (let [env     (stub-env (fn [_request]
                            {:status 401
                             :body   (json/write-str {:error {:message "bad key"}})}))
        context {:messages [{:role :user :content "hi" :timestamp 1}]}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"OpenAI completions request failed"
         (sut/complete env base-model context {:api-key "bad"})))))

(deftest complete-openai-compatible-without-api-key
  (let [seen-request (atom nil)
        model        (assoc base-model
                            :provider :openai-compatible
                            :base-url "http://localhost:11434/v1")
        env          (stub-env (fn [request]
                                 (reset! seen-request request)
                                 {:status 200
                                  :body   (json/write-str
                                           {:choices [{:finish_reason "stop"
                                                       :message       {:role    "assistant"
                                                                       :content "ollama ok"}}]
                                            :usage   {:prompt_tokens     3
                                                      :completion_tokens 2
                                                      :total_tokens      5}})}))
        out          (sut/complete env model {:messages [{:role :user :content "ping" :timestamp 1}]} {})
        request      @seen-request]
    (is
     (= {:role        :assistant
         :stop-reason :stop
         :content     [{:type :text :text "ollama ok"}]}
        (select-keys out [:role :stop-reason :content])))
    (is
     (= {:url  "http://localhost:11434/v1/chat/completions"
         :auth nil}
        {:url  (:url request)
         :auth (get-in request [:headers "Authorization"])}))))
