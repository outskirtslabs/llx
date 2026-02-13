(ns llx.agent.runtime.api
  (:refer-clojure :exclude [reset!])
  (:require
   [llx.agent.runtime :as runtime]))

(defn create-runtime
  "Creates an agent runtime.

  See [[llx.agent.runtime/create-runtime]] for options and return semantics."
  [opts]
  (runtime/create-runtime opts))

(defn state
  [rt]
  (runtime/state rt))

(defn prompt!
  [rt message-or-messages]
  (runtime/prompt! rt message-or-messages))

(defn continue!
  [rt]
  (runtime/continue! rt))

(defn steer!
  [rt message-or-messages]
  (runtime/steer! rt message-or-messages))

(defn follow-up!
  [rt message-or-messages]
  (runtime/follow-up! rt message-or-messages))

(defn abort!
  [rt]
  (runtime/abort! rt))

(defn wait-for-idle
  [rt]
  (runtime/wait-for-idle rt))

(defn reset!
  [rt]
  (runtime/reset! rt))

(defn close!
  [rt]
  (runtime/close! rt))

(defn subscribe
  [rt handler]
  (runtime/subscribe rt handler))

(defn set-steering-mode!
  [rt mode]
  (runtime/set-steering-mode! rt mode))

(defn set-follow-up-mode!
  [rt mode]
  (runtime/set-follow-up-mode! rt mode))

(defn clear-steering-queue!
  [rt]
  (runtime/clear-steering-queue! rt))

(defn clear-follow-up-queue!
  [rt]
  (runtime/clear-follow-up-queue! rt))

(defn clear-all-queues!
  [rt]
  (runtime/clear-all-queues! rt))

(defn has-queued-messages?
  [rt]
  (runtime/has-queued-messages? rt))
