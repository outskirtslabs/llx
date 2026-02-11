(ns llx.ai.event-stream-test
  (:require
   [clojure.test :refer [deftest is]]
   [llx.ai.event-stream :as sut]))

(set! *warn-on-reflection* true)

(defn- eventually
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if (pred)
        true
        (if (< (System/currentTimeMillis) deadline)
          (do
            (Thread/sleep 5)
            (recur))
          false)))))

(deftest stream-contract-basic
  (let [st (sut/create)]
    (is (sut/stream? st))
    (is (false? (sut/closed? st)))
    (is (false? (sut/consumed? st)))))

(deftest consume-can-only-be-called-once
  (let [st (sut/create)]
    (sut/consume! st {})
    (is (true? (sut/consumed? st)))
    (let [ex (try
               (sut/consume! st {})
               (catch clojure.lang.ExceptionInfo e e))]
      (is (= :llx/stream-already-consumed (:type (ex-data ex)))))))

(deftest consume-triggers-deferred-start-once
  (let [starts* (atom 0)
        st      (sut/create {:start-fn (fn [] (swap! starts* inc))})]
    (sut/consume! st {})
    (is (eventually #(= 1 @starts*) 200))
    (let [ex (try
               (sut/consume! st {})
               (catch clojure.lang.ExceptionInfo e e))]
      (is (= :llx/stream-already-consumed (:type (ex-data ex)))))
    (is (= 1 @starts*))))

(deftest consume-receives-events-result-close
  (let [st      (sut/create)
        events* (atom [])
        result* (atom nil)
        close*  (atom nil)
        result  {:role        :assistant
                 :content     [{:type :text :text "ok"}]
                 :api         :openai-completions
                 :provider    :openai
                 :model       "gpt-4o-mini"
                 :usage       {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                               :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                 :stop-reason :stop
                 :timestamp   0}
        close   {:reason :done :error nil :timestamp-ms 0}]
    (sut/consume! st {:on-event  (fn [event] (swap! events* conj event))
                      :on-result (fn [assistant-message] (reset! result* assistant-message))
                      :on-close  (fn [close-meta] (reset! close* close-meta))})
    (sut/emit-event! st {:type :start})
    (sut/emit-event! st {:type :text-delta :text "hello"})
    (sut/emit-result! st result)
    (sut/close! st close)
    (is (eventually #(= [{:type :start}
                         {:type :text-delta :text "hello"}]
                        @events*) 200))
    (is (= result @result*))
    (is (= close @close*))
    (is (true? (sut/closed? st)))))

(deftest cancel-is-idempotent-and-clears-consumer
  (let [cancel-count* (atom 0)
        close*        (atom [])
        st            (sut/create {:cancel-fn (fn [] (swap! cancel-count* inc))})]
    (sut/consume! st {:on-close (fn [close-meta]
                                  (swap! close* conj close-meta))})
    (sut/cancel! st)
    (sut/cancel! st)
    (is (eventually #(= 1 @cancel-count*) 200))
    (is (eventually #(= 1 (count @close*)) 200))
    (is (= :cancelled (:reason (first @close*))))
    (is (true? (sut/closed? st)))
    (is (nil? (:consumer @(:state* st))))))
