# Phase-1 UAL producer fixtures

These fixtures record our reading of the pinned draft schemas and UAL 1.0. The Rust tests are regression guards, not proof of conformance or verified properties; no Lean process or solver runs in them. `scripts/ual/e2e.sh` regenerates these sources with a real environment and checks the pinned consumer. See [the UAL-13 results](../../../../../docs/ual/e2e-results.md), including the unresolved exhaustion-verdict discrepancy.

## Running

```sh
cargo test -p aiken --test ual_conformance
```

The existing `cargo test --workspace` CI job runs every case. No workflow changes or optional environment flags are needed. Each CLI invocation uses an isolated dependency-free temporary project, HOME and XDG directories. The harness fixes `aiken.toml`'s modification time to `2026-11-04T00:00:00Z`, because the producer derives the assurance creation date from it.

| Project | Contract |
| --- | --- |
| `positive/native_function` | Data integer arguments, native Bool result, predicate fragment and `outbids` property |
| `positive/universal_validator` | Spend and else scopes, lowered purpose premises, universal minimum parameter |
| `positive/falsified` | Equal bids claimed to outbid; `old = proposed = 0` is the intended counterexample, not a phase-1 verdict |
| `positive/recursive_data` | Recursive Tree definitions and an identity function applied to valid Data-encoded leaves |
| `negative/*` | Unknown scope, wrong module, missing project fragment helper, opaque argument, missing steps, invalid semantics, mapped ID collision, both execution-setting mismatches, unterminated block |

The source-edit regression changes the native function from strict to non-strict comparison and checks that its compiled-code SHA-256 and checking-context digest both change while the companion blueprint remains unchanged.

Function fixtures include real unscoped companion validators: the pinned compiled-interface schema requires at least one validator. Propositions use Unicode ordering operators where needed; Aiken function bodies use Aiken operators.

## Goldens and sharing

Each positive project's `golden/` is a standalone export:

```text
golden/
  environment.json
  assurance/
    plutus.json
    assurance.json
    checking-<property>.json
```

Share `positive/native_function/golden/` as one directory with the Blaster team. Its relative artifact URIs resolve within that directory. The environment manifest is **fake**, with fixed synthetic tool/module digests and Lean 4.24.0 metadata. It is not evidence about a loaded consumer. Consumer testing requires a real environment and regeneration, not treating these hashes as trusted identities.

Tests validate both committed and emitted cross-document/function/environment hashes, compare the complete output filename set, and compare serialized bytes. Only the compiler's Git commit suffix and its consequent blueprint digest are normalized after binding validation; compiler release version, compiled code, schema/definition graphs, propositions, all context bytes and all other hashes remain pinned. This prevents a new commit alone from invalidating fixtures. There is no automatic update mode.

To deliberately regenerate one project, first build the CLI with `cargo build -p aiken`. Copy its `aiken.toml` and `validators/` into an otherwise empty scratch directory, copy the shared `environment.json` beside them, and set the copied manifest's mtime with `touch -d '2026-11-04 00:00:00 UTC' aiken.toml`. From that directory run the built `aiken verify generate --environment environment.json`. Review every emitted document, then replace only that project's `golden/assurance/` with those files and retain the exact environment bytes at `golden/environment.json`. Never regenerate expected output inside a test. Any intentional compiler change must be reviewed with its code/hash/interface/proposition consequences before updating a golden.

Missing-fragment coverage concerns a qualified reference to an existing project predicate module that is absent from the property's import closure. Arbitrary unknown Lean names, types and elaboration remain the consumer's responsibility; this is not a producer-side Lean type checker.
