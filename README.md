# llx

> Unified LLM API and agent runtime for Clojure, ClojureScript, and Clojure Dart

Project Status: [Experimental](https://docs.outskirtslabs.com/open-source-vital-signs#experimental)

## llx.ai

> Unified LLM client for supported providers with cross-provider context handoff for CLJ, CLJS, and CLJD

My goal with `llx.ai` is to be able to switch models across different providers mid-context. 
`llx.ai` keeps context portable across providers and models by normalizing messages, stream events, tool-call state, and serialized context (as far as each provider allows).

In practice, that lets you switch models, including across providers, in the middle of a conversation without rebuilding context by hand.
That handoff pipeline is a core design goal and one of the main things that distinguishes `llx` from thinner wrappers.

`llx.ai` targets JVM Clojure, ClojureScript, and ClojureDart. The latter is still a work in progress, but I hope to get it there eventually.

Under the hood, `llx.ai` is built around the four API families that cover most of the LLM ecosystem: OpenAI Completions, OpenAI Responses, Anthropic Messages, and Google Generative AI.
`llx.ai` absorbs each provider's quirks around streaming, tool calls, reasoning traces, and model behavior to present a unified API.

This unified API is exposed under the entrypoints `ol.llx.ai/complete` and `ol.llx.ai/stream` and the default choice for most callers. 
The provider specific escape hatch is available with `ol.llx.ai/complete*` and `ol.llx.ai/stream*`. Those let you work directly with provider-specific options.

`llx.ai` does not use any Java libraries/sdks. Inference provider HTTP APIs are used directly.

## llx.agent

`llx.agent` is an agent runtime that sits on top of `llx.ai`.
`llx.ai` handles talking to an LLM and `llx.agent` handles running the multi-turn agent loop: streaming inference, executing tools, managing conversation state, and reacting to steering interrupts.

The core is a pure finite state machine.
Side effects like LLM calls and tool execution are described as inert data and interpreted separately, keeping the loop logic testable.

Grab-bag of features:

- Tools run concurrently and can be aborted mid-execution.
- The steering queue lets you inject messages while the agent is still running and it will interrupt, skip pending tools, and re-prompt.
- The follow-up queue lets you queue messages for after the current turn completes.
- The event subscription exposes a stream of structured events (turn lifecycle, message deltas, tool progress) for rendering UI or logging.
- Agent state can be snapshotted (BYO durability) and rehydrated for persistence across sessions.
- Custom message roles let you store application-specific data in the conversation history that gets filtered before reaching the LLM.

```clojure
(require '[ol.llx.ai :as ai]
         '[ol.llx.agent :as agent]
         '[promesa.exec.csp :as sp])

(def read-tool
  {:name         "read_file"
   :description  "Read the contents of a file at the given path."
   :input-schema [:map [:path :string]]
   :execute      (fn [_id {:keys [path]} _abort _on-update]
                   {:content [{:type :text :text (slurp path)}]})})

(def a (agent/create-agent {:model         (ai/get-model :anthropic "claude-sonnet-4-6")
                            :system-prompt "You are a helpful assistant."
                            :tools         [read-tool]}))

;; subscribe to events
(let [ch (agent/subscribe a)]
  (future
    (loop []
      (when-let [ev (sp/take! ch)]
        (println (:type ev))
        (recur)))))

(agent/prompt a [{:role :user :content "read README.md" :timestamp 1}])

(agent/close a)
```

## License: European Union Public License 1.2

Copyright © 2026 Casey Link

Distributed under the [EUPL-1.2](https://spdx.org/licenses/EUPL-1.2.html).
