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
  <!-- BLASTER_REVIEW_RISK(relational_fixture_overclaim): this fixture shows an `image` relation, but it does not currently contain an explicit existential-intermediate node. -->
  - readable `DomainRel::Image` / existential-intermediate shape
  - no fallback to an unconstrained output predicate
- [`04-fail-once-witness.json`](./04-fail-once-witness.json)
  - `fail_once`
  <!-- BLASTER_REVIEW_RISK(witness_fixture_explanation_gap): the fixture JSON's `explanation` field is shorter than the description here and does not itself restate the bounded-runner caveat. -->
  - replay-backed witness validation
  - witness seed/replay metadata
- [`05-sampler-fallback-partial.json`](./05-sampler-fallback-partial.json)
  - local sampler fallback for an opaque child/custom sampler path
  - `Partial`, not fake universal success
- [`06-scenario-trace-partial.json`](./06-scenario-trace-partial.json)
  - bounded scenario/state-machine trace metadata
  <!-- BLASTER_REVIEW_RISK(scenario_fixture_nested_blockers): scenario blockers are not confined to top-level `obligations_open`; consumers also need nested scenario metadata and compatibility diagnostics. -->
  - precise `Partial` classification with blockers/open obligations

<!-- BLASTER_REVIEW_RISK(fixtures_tests_partial_coverage): the fixture tests deserialize and spot-check selected fields, but they do not exhaustively assert every field in each JSON example. -->
These files are checked by tests so that documentation examples stay synchronized with the current JSON contract.
