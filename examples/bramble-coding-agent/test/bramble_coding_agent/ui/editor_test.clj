(ns bramble-coding-agent.ui.editor-test
  (:require
   [bramble-coding-agent.ui.editor :as sut]
   [clojure.test :refer [deftest is]]
   [ol.tendril.event.key :as key]))

(defn- key-event
  [& {:keys [kind code value modifiers]
      :or   {kind      ::key/press
             modifiers #{}}}]
  {::key/kind      kind
   ::key/code      code
   ::key/value     value
   ::key/modifiers modifiers})

(deftest enter-is-submit-test
  (is (true? (sut/submit-event? (key-event :code ::key/enter))))
  (is (false? (sut/submit-event? (key-event :code ::key/enter
                                            :modifiers #{::key/shift}))))
  (is (false? (sut/submit-event? (key-event :value \a)))))

(deftest shift-enter-inserts-newline-test
  (let [state  (sut/init :composer "hello")
        state' (sut/handle-event state (key-event :code ::key/enter
                                                  :modifiers #{::key/shift}))]
    (is (= "hello\n" (sut/text state')))
    (is (= 1 (:row (sut/cursor-position state'))))))

(deftest paste-preserves-multiple-lines-test
  (let [state  (sut/init :composer)
        state' (sut/handle-event state {:type :paste :content "alpha\nbeta\ngamma"})]
    (is (= "alpha\nbeta\ngamma" (sut/text state')))
    (is (= 2 (:row (sut/cursor-position state'))))))

(deftest up-and-down-move-between-lines-test
  (let [state (sut/init :composer "one\ntwo\nthree")
        down  (-> state
                  (sut/set-cursor-row-col 0 1)
                  (sut/handle-event (key-event :code ::key/down)))
        up    (sut/handle-event down (key-event :code ::key/up))]
    (is (= 1 (:row (sut/cursor-position down))))
    (is (= 0 (:row (sut/cursor-position up))))))

(deftest visible-tail-shows-last-lines-test
  (let [state (sut/init :composer "l1\nl2\nl3\nl4")]
    (is (= ["l3" "l4"] (sut/visible-lines state 2)))))
