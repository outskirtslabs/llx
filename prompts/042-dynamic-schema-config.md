# Dynamic Schema Config Implementation Plan

Goal: Allow a running agent to accept user schema registry updates and custom message schema updates through a single `set-schema-config` API without restarting the agent.

Architecture: Keep one logical schema system by storing only the user-controlled schema inputs in agent state and deriving the effective active registry on demand.
Architecture: The derived registry must always include Malli defaults, LLX schemas, the current `custom-message-schemas` dispatch map, and the current user `schema-registry`.

Tech Stack: Clojure, Malli, Promesa CSP, clojure.test, Kaocha.

Related: Builds on `prompts/028-agent-opts-parity.md` and `prompts/029-tool-execution.md`.

## Problem statement

The current agent runtime captures an effective schema registry at `create-agent` time and keeps it fixed for the life of the agent.

That design blocks the REPL workflow because tools can already be replaced at runtime, but tool input schemas and custom message schemas cannot be changed without restarting the agent.

`custom-message-schemas` and the user `schema-registry` are coupled inputs because a custom message dispatch key is only valid when the named schema exists in the active registry.

The public API should not ask the user to construct or replace the final effective registry because LLX must always preserve Malli defaults and its own required schemas.

The runtime should instead treat `:schema-registry` and `:custom-message-schemas` as user-owned inputs and always derive the effective registry from those inputs plus LLX base schemas.

The new runtime setter should be `set-schema-config`.

`set-schema-config` should be the only mutator for schema configuration.

This repository is pre-release, so this design should prefer the cleanest direct API and state model rather than preserving old behavior.

Do not add compatibility wrappers, dual-write state, fallback code paths, deprecation layers, migration shims, or legacy aliases.

Do not add runtime guard code that audits or blocks user overrides of LLX schema keys.

If the user mutates their schema inputs in a way that breaks runtime behavior, that failure is their responsibility.

`nil` for either field should mean ignore that field and leave the current user-owned value unchanged.

The effective registry should never be directly set by user code.

```text
user schema-registry -----------+
                                |
user custom-message-schemas ----+--> derive active registry --> validate messages
                                |                           --> validate agent commands and effects
LLX + Malli base schemas -------+                           --> coerce and validate tool inputs
```

## Testing Plan

Use @test-driven-development and write all failing tests before changing implementation behavior.

Add JVM tests in `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc` that prove `set-schema-config` updates custom message validation on a live agent.

Add JVM tests in `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc` that prove `set-schema-config` ignores `nil` fields and preserves the current user-owned schema inputs.

Add JVM tests in `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc` that prove the active registry still includes LLX canonical message schemas after user config is replaced with `{}`.

Add JVM tests in `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc` that prove `rehydrate-agent` rebuilds the active registry from persisted schema config in state.

Add JVM tests in `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent/fx_test.cljc` that prove tool input coercion uses the current derived registry after `set-schema-config`.

Add JVM tests in `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/ai/utils/tool_validation_test.cljc` only if the implementation changes the public helper contract or registry derivation helper in a meaningful way.

Run `bb test:jvm --focus ol.llx.agent-test`.

Expected result is green output for the new runtime schema configuration tests.

Run `bb test:jvm --focus ol.llx.agent.fx-test`.

Expected result is green output for the tool execution and live registry coercion tests.

Run `bb test:jvm --focus ol.llx.agent.loop-test`.

Expected result is green output for loop command handling after the new command is added.

Run `bb lint /home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc /home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/schema.cljc /home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/loop.cljc /home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/fx/tools.cljc /home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc /home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent/fx_test.cljc`.

Expected result is `errors: 0, warnings: 0`.

NOTE: I will write *all* tests before I add any implementation behavior.

## Implementation Plan

1. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/schema.cljc`, add a schema for user-owned schema configuration that contains `:schema-registry` and `:custom-message-schemas`.

2. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/schema.cljc`, add `:ol.llx.agent.command/set-schema-config` to `:ol.llx.agent/command-type`, add its command schema, and register it in `:ol.llx.agent/command`.

3. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/schema.cljc`, extend `:ol.llx.agent.loop/state` with persisted user-owned `:schema-registry` and `:custom-message-schemas` fields using `{}` defaults.

4. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/schema.cljc`, add a pure helper that derives the active registry from a schema config map rather than from creation-time opts.

5. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, replace the current `agent-schema-registry` creation-time helper with a helper that accepts the current user-owned schema config and returns the derived active registry.

6. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, seed the initial loop state with raw user-owned `:schema-registry` and `:custom-message-schemas` instead of only embedding them in the startup registry.

7. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, update `create-agent` to validate startup opts with the derived initial registry and then store the raw user-owned schema config in the created state.

8. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, update `rehydrate-agent` to derive the active registry from the state snapshot’s schema config before validating the rehydrated state.

9. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, replace `validate-with-agent-registry!` with a helper that reads the current public state from `:state_`, derives the active registry, and validates against that live registry.

10. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, add `set-schema-config` as the only public schema mutator.

11. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, make `set-schema-config` build the next complete user-owned schema config by reading current state and ignoring any incoming `nil` fields before dispatch.

12. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, validate the proposed next schema config against the derived next active registry before dispatch so invalid updates fail before mutating runtime state.

13. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/loop.cljc`, add handling for `:ol.llx.agent.command/set-schema-config` and make it replace both persisted user-owned schema config fields atomically in state.

14. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/schema.cljc` and `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, remove the fixed effective-registry assumption from `:ol.llx.agent/env` and runtime construction so there is no stale registry field left behind as dead state.

15. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/fx.cljc`, derive the active registry from the current public state before validating effects.

16. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent/fx/tools.cljc`, pass the live derived registry into tool input validation so tool coercion reflects the current schema config.

17. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/ai/impl/utils/tool_validation.cljc`, keep the explicit registry arity and use it as the sole runtime source for Malli coercion and explain data.

18. In `/home/ramblurr/src/github.com/outskirtslabs/llx/src/ol/llx/agent.cljc`, document that `set-schema-config` is allowed in any phase, applies to subsequent validation and tool execution, and does not retroactively revalidate already queued messages or already running tool executions.

19. In `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc`, write a failing test that creates an agent without a custom message role, proves the message is rejected, then calls `set-schema-config`, and proves the same message is accepted without recreating the agent.

20. In `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc`, write a failing test that starts with a custom role and then calls `set-schema-config` with `{:custom-message-schemas {}}` while preserving the user schema registry and proves the role is rejected while canonical messages still validate.

21. In `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc`, write a failing test that passes `nil` for `:schema-registry` or `:custom-message-schemas` and proves the omitted field is preserved.

22. In `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent_test.cljc`, write a failing test that rehydrates a state containing user schema config and proves validation still works after rehydration.

23. In `/home/ramblurr/src/github.com/outskirtslabs/llx/test/ol/llx/agent/fx_test.cljc`, write a failing test that updates schema config on a live agent and proves a tool input schema alias begins coercing against the new registry without restarting the agent.

24. Run the focused JVM tests and confirm they fail for the missing runtime schema-config behavior rather than for malformed test setup.

25. Implement the minimal production changes in the files above until the new tests pass.

26. Run the focused JVM tests again and confirm the new runtime behavior is green.

27. Run the lint command and fix any remaining namespace or arity issues.

## Behavior Rules

The active registry is always derived and is never directly stored as the user-facing source of truth.

The implementation should make the direct model true and should not preserve obsolete API or state shapes for compatibility.

The persisted state stores only user-owned additions.

`{}` for `:schema-registry` means the user has no extra schemas.

`{}` for `:custom-message-schemas` means the user has no extra custom message roles.

Those empty maps must not remove Malli defaults or LLX built-in schemas from the active registry.

`nil` in the public `set-schema-config` call means ignore that field and preserve the current persisted user-owned value.

Runtime changes apply to future validation and future tool execution only.

No convenience wrappers should be added.

## Testing Details

The core behavior tests should prove that a single long-lived agent can accept a new schema config at runtime and immediately validate newly allowed custom messages and tool inputs without recreation.

The tests should also prove that replacing user-owned config with `{}` only removes user additions and never breaks LLX canonical message validation or built-in runtime schemas.

The nil-handling tests should prove behavior, not map shape, by verifying that the previously active message role or schema alias still works after a partial update.

## Implementation Details

- Use @clojure-malli for the registry derivation and coercion decisions.
- Keep one logical registry model by deriving active state from user inputs plus LLX base schemas.
- Remove or replace outdated schema-registry assumptions directly instead of layering compatibility behavior on top.
- Treat `set-schema-config` as an atomic replacement of both persisted user-owned config fields after nil-elision.
- Do not keep a stale effective registry in env or state just to ease the transition.
- Validate proposed next config before dispatch so a bad update does not poison runtime state.
- Keep the public API consistent with existing setters by adding only `set-schema-config`.
- Preserve pre-release pragmatism and do not add compatibility shims, migration layers, fallback aliases, or tech debt for old behavior.
- Prefer pure helpers for deriving active registry so the same logic is reused by create, rehydrate, public validation, and fx validation.
- Keep tool validation explicitly registry-driven and avoid any fallback to stale env-captured registries.
- Do not add protective runtime assertions that police whether user schema inputs override LLX keys.
- Update docstrings where they describe schema config behavior or agent options.

## Question

No blocking question remains.

`set-schema-config` should be accepted in any phase and should take effect on the next validation boundary.

Already-running operations should continue using the registry they started with.

---

## Decision note after implementation

The implementation intentionally deviates slightly from the original "future validation only" wording for one boundary: `set-schema-config` validates the next complete public state before dispatch. As a result, an update that removes the schema for custom messages already persisted in `:messages` is rejected instead of leaving history invalid under the new active registry.

This keeps the persisted agent state self-consistent with the active schema config. We may revisit this if a real workflow needs schema config changes to ignore historical custom messages while still applying only to future validation and tool execution.
