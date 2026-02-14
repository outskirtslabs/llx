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
  ([rt message-or-messages]
   (runtime/prompt! rt message-or-messages))
  ([rt text images]
   (runtime/prompt! rt text images)))

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

(defn set-system-prompt!
  [rt system-prompt]
  (runtime/set-system-prompt! rt system-prompt))

(defn set-model!
  [rt model]
  (runtime/set-model! rt model))

(defn set-thinking-level!
  [rt thinking-level]
  (runtime/set-thinking-level! rt thinking-level))

(defn set-tools!
  [rt tools]
  (runtime/set-tools! rt tools))

(defn replace-messages!
  [rt messages]
  (runtime/replace-messages! rt messages))

(defn append-message!
  [rt message]
  (runtime/append-message! rt message))

(defn clear-messages!
  [rt]
  (runtime/clear-messages! rt))

(defn session-id
  [rt]
  (runtime/session-id rt))

(defn set-session-id!
  [rt session-id]
  (runtime/set-session-id! rt session-id))

(defn thinking-budgets
  [rt]
  (runtime/thinking-budgets rt))

(defn set-thinking-budgets!
  [rt budgets]
  (runtime/set-thinking-budgets! rt budgets))

(defn max-retry-delay-ms
  [rt]
  (runtime/max-retry-delay-ms rt))

(defn set-max-retry-delay-ms!
  [rt max-retry-delay-ms]
  (runtime/set-max-retry-delay-ms! rt max-retry-delay-ms))

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
