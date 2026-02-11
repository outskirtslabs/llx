(ns llx.ai.promesa.csp
  "Optional Promesa CSP adapter for [[llx.ai.event-stream]].

  This namespace provides channel/promise-oriented helpers on top of the
  callback-first stream contract:

  - [[stream->chan]] emits ordered LLX events to a CSP channel.
  - [[result]] resolves to the terminal assistant message.
  - [[drain]] resolves to all observed events.
  - [[stream]] is the unified-options convenience wrapper around [[llx.ai/stream]].
  - [[stream*]] and [[stream+]] are convenience wrappers around [[llx.ai/stream*]].

  Buffering defaults to a bounded sliding buffer (`64`) so slow consumers drop
  oldest events first under backpressure.

  Dependency boundary:

  `llx.ai` core does not require Promesa. Consumers that require this namespace
  must include Promesa on the classpath."
  (:require
   [llx.ai :as ai]
   [llx.ai.event-stream :as stream]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- normalize-buffer
  [buf]
  (if (and (int? buf) (pos? buf))
    (sp/sliding-buffer buf)
    buf))

(defn stream->chan
  "Attaches to `st` and returns a Promesa CSP channel of ordered stream events.

  This function consumes `st` via [[llx.ai.event-stream/consume!]], so it must be the
  first and only consumer attachment for that stream handle.

  Options:

  | key    | description                                                                                                              |
  |--------|--------------------------------------------------------------------------------------------------------------------------|
  | `:buf` | Channel buffer. Accepts a Promesa buffer instance or a positive integer. Integers are normalized to `sp/sliding-buffer`. |
  "
  ([st]
   (stream->chan st {}))
  ([st {:keys [buf]
        :or   {buf (sp/sliding-buffer 64)}}]
   (let [ch (sp/chan :buf (normalize-buffer buf))]
     (stream/consume! st
                      {:on-event (fn [event]
                                   (sp/put ch event))
                       :on-close (fn [_close-meta]
                                   (sp/close ch))})
     ch)))

(defn result
  "Consumes `st` and returns a deferred that resolves to the terminal assistant message.

  Rejects with `ex-info` (`:type :llx/stream-closed`) when the stream closes
  before a terminal result is emitted."
  [st]
  (let [p* (p/deferred)]
    (stream/consume! st
                     {:on-result (fn [assistant-message]
                                   (p/resolve p* assistant-message))
                      :on-close  (fn [close-meta]
                                   (when-not (p/resolved? p*)
                                     (p/reject p*
                                               (ex-info "Stream closed before terminal result"
                                                        {:type       :llx/stream-closed
                                                         :close-meta close-meta}))))})
    p*))

(defn drain
  "Consumes `st` and returns a deferred that resolves to all observed events in order."
  [st]
  (let [events* (atom [])
        p*      (p/deferred)]
    (stream/consume! st
                     {:on-event (fn [event]
                                  (swap! events* conj event))
                      :on-close (fn [_close-meta]
                                  (p/resolve p* @events*))})
    p*))

(defn stream
  "Calls [[llx.ai/stream]] and returns a CSP channel via [[stream->chan]]."
  [env model context opts]
  (-> (ai/stream env model context opts)
      (stream->chan)))

(defn stream*
  "Calls [[llx.ai/stream*]] and returns a CSP channel via [[stream->chan]]."
  [env model context opts]
  (-> (ai/stream* env model context opts)
      (stream->chan)))

(defn stream+
  "Calls [[llx.ai/stream*]] and returns combined stream helpers.

  Returns a map with:

  - `:stream` underlying LLX stream handle
  - `:chan` CSP channel of ordered events
  - `:result` deferred terminal assistant message
  - `:cancel!` idempotent cancel function delegating to [[llx.ai.event-stream/cancel!]]"
  [env model context opts]
  (let [st      (ai/stream* env model context opts)
        ch      (sp/chan :buf (sp/sliding-buffer 64))
        result* (p/deferred)]
    (stream/consume! st
                     {:on-event  (fn [event]
                                   (sp/put ch event))
                      :on-result (fn [assistant-message]
                                   (p/resolve result* assistant-message))
                      :on-close  (fn [close-meta]
                                   (sp/close ch)
                                   (when-not (p/resolved? result*)
                                     (p/reject result*
                                               (ex-info "Stream closed before terminal result"
                                                        {:type       :llx/stream-closed
                                                         :close-meta close-meta}))))})
    {:stream  st
     :chan    ch
     :result  result*
     :cancel! #(stream/cancel! st)}))
