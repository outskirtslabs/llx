# llx

> Unified LLM client for supported providers with cross-provider context handoff for CLJ, CLJS, and CLJD

Cross-provider context handoff is at the center of `llx`.
It keeps context portable across providers and models by normalizing messages, stream events, tool-call state, and serialized context (as far as each provider allows).
In practice, that lets you switch models, including across providers, in the middle of a conversation without rebuilding context by hand.
That handoff pipeline is a core design goal and one of the main things that distinguishes `llx` from thinner wrappers.

`llx` targets JVM Clojure, ClojureScript, and ClojureDart. The latter is still a work in progress, but I hope to get it there soon.

Under the hood, `llx` is built around the four API families that cover most of the LLM ecosystem: OpenAI Completions, OpenAI Responses, Anthropic Messages, and Google Generative AI.
The basic shape isn't that big. The real work is absorbing each provider's many small quirks around streaming, tool calls, reasoning traces, and model behavior so your application code does not have to.

This unified API is exposed under the entrypoints `ol.llx.ai/complete` and `ol.llx.ai/stream` and the default choice for most callers. 
If you need provider-specific options, `ol.llx.ai/complete*` and `ol.llx.ai/stream*` let you work directly with them.

Project Status: [Experimental](https://docs.outskirtslabs.com/open-source-vital-signs#experimental)

## License: European Union Public License 1.2

Copyright © 2026 Casey Link

Distributed under the [EUPL-1.2](https://spdx.org/licenses/EUPL-1.2.html).
