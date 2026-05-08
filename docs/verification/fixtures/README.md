# Verification golden fixtures

These files are small, stable JSON examples for the user-facing verification contract.

They are intentionally narrower than full end-to-end project outputs. Each fixture focuses on one verification story so users and maintainers can inspect the shape without reading generated Lean.

Current fixtures:

- [`01-primitive-normal-bool.json`](./01-primitive-normal-bool.json)
  - universal `normal`
  - `return_mode = bool`
  - primitive exact domain
  - `SolverValidated`
- [`02-primitive-fail-void.json`](./02-primitive-fail-void.json)
  - universal `fail`
  - `return_mode = void`
  - trusted primitive domain
  - `SolverValidated`
- [`03-relational-custom-fuzzer.json`](./03-relational-custom-fuzzer.json)
  - relational custom fuzzer lowering
  - readable `DomainRel::Image` / existential-intermediate shape
  - no fallback to an unconstrained output predicate
- [`04-fail-once-witness.json`](./04-fail-once-witness.json)
  - `fail_once`
  - replay-backed witness validation
  - witness seed/replay metadata
- [`05-sampler-fallback-partial.json`](./05-sampler-fallback-partial.json)
  - local sampler fallback for an opaque child/custom sampler path
  - `Partial`, not fake universal success
- [`06-scenario-trace-partial.json`](./06-scenario-trace-partial.json)
  - bounded scenario/state-machine trace metadata
  - precise `Partial` classification with blockers/open obligations

These files are checked by tests so that documentation examples stay synchronized with the current JSON contract.
