(ns llx.ai.promesa.csp-test
  (:require
   [clojure.test :refer [deftest is]]
   [llx.ai :as ai]
   [llx.ai.promesa.csp :as sut]
   [llx.ai.stream :as stream]
   [promesa.exec.csp :as sp]))

(set! *warn-on-reflection* true)

(def terminal-assistant
  {:role        :assistant
   :content     [{:type :text :text "done"}]
   :api         :openai-completions
   :provider    :openai
   :model       "gpt-4o-mini"
   :usage       {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                 :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
   :stop-reason :stop
   :timestamp   0})

(deftest stream->chan-preserves-order-and-closes
  (let [st    (stream/create)
        ch    (sut/stream->chan st)
        _     (stream/emit-event! st {:type :start})
        _     (stream/emit-event! st {:type :text-delta :text "a"})
        _     (stream/emit-event! st {:type :text-delta :text "b"})
        _     (stream/emit-result! st terminal-assistant)
        _     (stream/close! st {:reason :done :error nil :timestamp-ms 0})
        out-1 (sp/take! ch)
        out-2 (sp/take! ch)
        out-3 (sp/take! ch)
        out-4 (sp/take! ch)]
    (is (= [:start :text-delta :text-delta nil]
           [(some-> out-1 :type)
            (some-> out-2 :type)
            (some-> out-3 :type)
            out-4]))))

(deftest stream->chan-normalizes-int-buffer-to-sliding-behavior
  (let [st    (stream/create)
        ch    (sut/stream->chan st {:buf 1})
        _     (stream/emit-event! st {:type :text-delta :text "a"})
        _     (stream/emit-event! st {:type :text-delta :text "b"})
        _     (stream/emit-event! st {:type :text-delta :text "c"})
        _     (stream/close! st {:reason :done :error nil :timestamp-ms 0})
        _     (Thread/sleep 25)
        out-1 (sp/take! ch)
        out-2 (sp/take! ch)]
    (is (= "c" (:text out-1)))
    (is (nil? out-2))))

(deftest result-and-drain
  (let [st-result (stream/create)
        st-drain  (stream/create)]
    (let [result-p (sut/result st-result)]
      (stream/emit-event! st-result {:type :start})
      (stream/emit-event! st-result {:type :text-delta :text "ok"})
      (stream/emit-result! st-result terminal-assistant)
      (stream/close! st-result {:reason :done :error nil :timestamp-ms 0})
      (is (= terminal-assistant @result-p)))

    (let [drain-p (sut/drain st-drain)]
      (stream/emit-event! st-drain {:type :start})
      (stream/emit-event! st-drain {:type :text-delta :text "ok"})
      (stream/emit-result! st-drain terminal-assistant)
      (stream/close! st-drain {:reason :done :error nil :timestamp-ms 0})
      (is (= [:start :text-delta]
             (mapv :type @drain-p))))))

(deftest result-rejects-when-closed-without-terminal-result
  (let [st (stream/create)]
    (stream/close! st {:reason :ended :error nil :timestamp-ms 0})
    (is (thrown? Throwable @(sut/result st)))))

(deftest stream-wrappers-delegate-to-ai-stream-apis
  (let [simple-calls*   (atom 0)
        advanced-calls* (atom 0)]
    (with-redefs [ai/stream  (fn [_env _model _context _opts]
                               (swap! simple-calls* inc)
                               (stream/create))
                  ai/stream* (fn [_env _model _context _opts]
                               (swap! advanced-calls* inc)
                               (stream/create))]
      (let [simple-ch     (sut/stream {} {:id "m"} {:messages []} {})
            advanced-ch   (sut/stream* {} {:id "m"} {:messages []} {})
            stream+-value (sut/stream+ {} {:id "m"} {:messages []} {})]
        (is (= 1 @simple-calls*))
        (is (= 2 @advanced-calls*))
        (is (some? simple-ch))
        (is (some? advanced-ch))
        (is (stream/stream? (:stream stream+-value)))
        (is (fn? (:cancel! stream+-value)))))))
