# UAL integration: reviewer summary

This PR makes Aiken a UAL 1.0 **producer**, with a CLI that delegates checking to
an external, pinned PlutusCoreBlaster consumer. Users write explicit properties
about compiled UPLC, not proofs inferred from tests. Start with the
[user guide](user-guide.md); the [plan](plan.md), [decisions log](decisions-log.md)
and [real-consumer results](e2e-results.md) retain design rationale and evidence.

`aiken verify generate` emits a detached bundle under `assurance/`.
`aiken verify env` captures the consumer environment; `aiken verify check`
regenerates the bundle and runs the consumer. Ordinary `aiken build`, `check`
and `export` require neither Lean nor Z3. The ordinary root `plutus.json` is not
replaced by verification output.

## Architecture decisions

| ADR | Decision and review consequence |
| --- | --- |
| [0001: producer only](adr/0001-aiken-is-a-ual-producer-only.md) | Aiken produces claims and document bindings; the external consumer elaborates Lean and calls Blaster/Z3. The check command packages consumer-derived SMT evidence only after a successful run. No evidence comes from generation alone. |
| [0002: annotation comments](adr/0002-annotations-in-comments.md) | `//@ … @//` carries opaque Lean text. The lexer records spans, the formatter preserves annotation bytes, and the assurance pass resolves scopes after Aiken type checking. Lean syntax/elaboration remains the consumer's responsibility. |
| [0003: derived signature](adr/0003-derived-onchain-signature.md) | `@onchain` specifies execution settings, not encodings. Argument/result interfaces derive from Aiken's actual compiled representation; native list/pair results contain Data elements. |
| [0004: validator target](adr/0004-validator-is-the-target-purposes-are-premises.md) | Check the whole compiled validator, including dispatch. A handler suffix adds a premise to the complete proposition, not a separately compiled handler. Selecting a V3 invocation alone does not restrict the context. |
| [0005: no fuzzer lifting](adr/0005-no-fuzzer-to-quantifier-lift.md) | Sampling distributions do not define quantified domains. Properties are written explicitly; no test-to-property translation or old integration-branch implementation is used. |
| [0006: separate blueprint](adr/0006-dialect-blueprint-under-assurance.md) | The compiled-interface dialect lives at `assurance/plutus.json`; root `plutus.json` remains for existing off-chain tooling. |
| [0007: emitted IDs](adr/0007-emitted-target-ids.md) | `module/path.name` becomes `module_path__name` (also replacing module-path hyphens). Scopes use local Aiken names; Lean propositions use emitted IDs. Collisions are errors. This supersedes ADR-0004's illustrative dotted ID. |
| [0008: applied parameters](adr/0008-property-local-applied-parameters.md) | `validator.handler[CBOR_HEX,...]` binds all parameters per property; wrappers take runtime arguments only. Exact template applications, Flat values and deployed script hashes are context-bound. Universal claims remain available without brackets. |

Implementation boundaries: `aiken-lang` owns annotation trivia and decorators;
`aiken-project/src/assurance/` owns scope resolution, lowering and consumer
execution; `aiken-ual` owns document models, canonical serialization, schemas
and bindings; `aiken/src/cmd/verify/` exposes the commands. Setup, e2e and drift
tooling live in [`scripts/ual/`](../../scripts/ual/README.md).

## Defaults and limits in force

| Area | Current contract |
| --- | --- |
| Execution settings | **No default steps or semantics.** Each target declares positive `steps` and `semantics: c` or `e`. All targets of one property must agree. These are CEK steps, not ledger CPU/memory ex-units. E is post-Conway; C remains supported. |
| Checking context | Plutus V3, `acceptance: evaluation`, universal parameters by default. A scope's `[CBOR_HEX,...]` suffix selects fully applied mode. No property-template syntax is exposed. |
| Properties | Project-unique IDs, mandatory nonempty descriptions, targets in the property's own module. Predicate fragments follow Aiken `use` edges. |
| Purpose scopes | All six V3 invocations are emitted. Handler scopes insert bracketed existential premises; `else` conjoins negations of defined handlers. Unsuffixed and `else` scopes select `spend` as the calling convention, not as a context restriction. Multiple handler scopes conjoin premises, so mutually exclusive scopes can make a claim vacuous. |
| Generation | Requires `--environment PATH` or `--no-environment`. The latter is a write-free, environment-unbound dry run; `--environment PATH --dry-run` validates full bindings without writes. Existing dependency installations are required for read-only compilation. |
| Checking | Environment defaults to `environment.json` in the invocation directory; timeout defaults to 300 seconds for the consumer invocation. Trace level defaults to silent. Workspace members are checked separately and failures aggregate. |
| Consumer workspace | `[assurance].workspace` takes precedence over `AIKEN_ASSURANCE_WORKSPACE`. It points to the **CardanoLedgerApiBlaster checkout**, not the sibling root. TOML-relative paths resolve from the manifest; environment-variable-relative paths from the invocation directory. |
| Setup | `scripts/ual/workspace-setup.sh` uses `$HOME/ual-workspace`, overridden by `UAL_WORKSPACE` or `--workspace`. Aiken does not install or repair Lean, Z3 or consumer checkouts. |
| Environment capture | Default output `environment.json`, plus a separate `.run.json` sidecar. Capture preserves the consumer manifest's exact bytes. Revision/lock mismatches warn and are disclosed, not silently corrected or treated as pin enforcement. |
| Verdict protocol | The registered consumer revision uses exact per-property log lines and a consistent summary. Aiken's own non-TTY output is JSON, but no upstream JSON verdict-report support is claimed at this pin. |
| Success/evidence | Every property must be verified, the Lean exit must be zero, and publication must succeed. Unknown/duplicate/missing verdicts, empty bundles, inconsistent summaries, timeouts and errors fail closed. `verify check` removes stale evidence before the attempt and publishes `verified-assurance.json` last. |
| Pinned schema limitations | Even a function-only claim needs a real companion validator in its project's blueprint. Evidence cannot faithfully bind one property to distinct validator script hashes; publication fails explicitly. Use separate properties for those validators. |
| Reproducibility | The package owner supplies the assurance author; `aiken.toml`'s UTC modification date supplies its creation date. Preserve that date for byte-reproducible documents. Exact environment bytes and compiled artifacts are hash-bound. |

### Approved consumer inputs

The authoritative inputs are [`schemas/PIN`](../../crates/aiken-ual/schemas/PIN)
and the vendored [`assurance-workspace.json`](../../crates/aiken-ual/schemas/assurance-workspace.json),
not upstream branch heads or proposed PR pins:

| Component | Pin |
| --- | --- |
| PlutusCoreBlaster | `1c23de8f35fdc5decf6f126c7b56ed6649e4a57f` |
| Lean-blaster | `bc03bb402ce702984875bbf8ba51216bd6f843d7` |
| CardanoLedgerApiBlaster | `997b9caafb3f72f45d866929c2f561945d036e10` |
| UAL | `1f7ab847736e5fa53cadc2099300cdaf3c720c6b` (1.0) |
| Lean | `4.24.0` |
| Z3 | `5.1.0`, source baseline `ca39e58c525c5b03c2484f118ade2b92fae31215`, with `recfun-finder` |

Setup enforces the coordinated checkout revisions and checks the solver's
`recfun-finder` capability; it records the actual solver version/hash rather
than attesting to its source baseline. Direct CLI runs warn and continue on
installed revision/lock mismatches. The log-reader registration is tied to the
**vendored** consumer pin, not an enforcement check on the installed HEAD.
Use the setup script and inspect the trust report; neither a warning-free
capture nor a setup receipt is a proof of toolchain correctness.

## Trust boundary and review risks

**Verified means Blaster SMT, without a reconstructed Lean proof, relative to
the declared checking context and CEK budget.** It trusts the consumer's UPLC
import/encoding, Lean CEK and ledger models, Blaster translation/optimizer, Z3
binary and loaded environment. Hashes bind artifacts; they do not prove those
components correct. The pinned upstream build has a recorded
`CekMachine.lean:309` `declaration uses 'sorry'` warning; this PR neither removes
it nor claims an axiom-free upstream model.

For an exact-result `_returns` property, **falsified means no matching result
within the declared step budget for the counterexample**. An unfinished run
also makes that bounded claim false. Aiken does not reinterpret it as
inconclusive or claim a completed wrong result. `inconclusive` is the consumer's
solver `Undetermined`. The 2026-11-09 bounded-semantics decision supersedes the
historical blocked-signoff entries in the append-only log.

Evaluation acceptance is not phase-one ledger validity, execution-cost
acceptance, unbounded termination, or coverage of every possible contract
requirement. Premises and quantified domains are authored by the specifier;
a verified implication with impossible premises is still vacuous. A property
about a separately compiled function does **not** establish equivalence to its
inlined/optimized occurrence in a validator. The uncovered-validator warning
is advisory; deployed-script claims must scope the whole validator.

## End-to-end evidence

The [UAL-13 final run](e2e-results.md#final-run--2026-11-09-bounded-semantics-decision)
ran `scripts/ual/e2e.sh --output /tmp/ual-13-bounded-e2e`: **18/18 passed**, exit 0.
It used a fresh real environment and the approved consumers, not synthetic
fixture goldens. The historical run took 846.12 seconds, including 583.6 seconds
of workspace provisioning; these are observations, not performance benchmarks.

| Coverage | Observed result in that run |
| --- | --- |
| Native function (100 steps, E) | `outbids` verified; evidence published. |
| Universal validator (2500 steps, E) | Spend and fallback properties verified; evidence published. |
| Deliberately false equal bid | Falsified, `old = proposed = 0`; nonzero exit, no evidence. |
| Recursive Data function (2500 steps, C) | `preserves_leaf` verified; evidence published. |
| Equivalent match-based purpose premise | Both validator properties verified, as with bracketed existentials. Retained existentials; no discharge advantage was observed for match. |
| One-step exact-result claim | Falsified, `old = 0`, `proposed = 1`; nonzero exit, no evidence. A different verdict fails the runner. |
| Ten source negatives | Expected producer diagnostics; no bundle/evidence. Includes scope, fragment, interface, settings, ID collision and unterminated-block failures. |
| Two tampered bundles | Altered function code and mismatched digest both rejected by the consumer. |

The runner checks exact property sets, verdicts, exit/evidence agreement,
counterexamples, trust warnings and root-blueprint preservation. Tamper cases
invoke the generated Lean driver directly, because `verify check` would
regenerate the inputs. Synthetic golden bundles exercise producer conformance;
they are not evidence of SMT verification. See the results document for original
namespace/binder failures and the individually reviewed snapshot corrections.

### UAL-16 fresh documentation validation — 2026-11-09

`scripts/ual/e2e.sh --output /tmp/ual-16-e2e` also passed **18/18**, exit 0,
with the same verdicts and evidence rules above. Total wall time was 690.32
seconds, including 514.2 seconds provisioning the pinned workspace. All three
upstream Game properties verified during setup; the existing
`CekMachine.lean:309` `sorry` warning remained visible.

The run used Aiken `v1.1.24+dcc4b4c`, Lean 4.24.0, the three approved checkout
revisions above, and Z3 5.1.0 at `ca39e58…`. The observed Z3 binary SHA-256 was
`6a9c23e251e44bffab2f5e7fe6d63ce247a891062d35a5d7a4836694b8776aea`;
the captured environment SHA-256 was
`65bbdc2e64b4c2e51bb82762199b5a3927429615ed235c11036a93935f473536`.
`identity.json`, `results.json`, inputs and raw logs were retained locally under
`/tmp/ual-16-e2e`; this path is a local run artifact, not a portable repository link.

The user guide's §4 shell example was executed separately from its documented
source text in a temporary directory: environment capture, unbound dry run,
bound generation and checking all exited zero. `outbids` reported `verified`,
with no warnings/errors and fresh evidence. The temporary example was removed.
All 51 local documentation links and their Markdown heading anchors resolved.
The 28 UAL Python script regressions and `bash -n scripts/ual/e2e.sh` also passed.

All required repository checks passed in this documentation run:

```sh
cargo build --workspace --all-targets
cargo build --workspace
cargo fmt --all -- --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test --workspace
aiken-acceptance
```

Acceptance ran through the bounded wrapper, at most two Aiken processes.
This ticket changes only documentation/changelog content; no tests, snapshots,
goldens, pins, CI/release configuration or protected implementation files were edited.

## Remaining upstream dependencies

Public PR pages were checked on **2026-11-09**. These are open dependencies for
future improvements, not prerequisites to use the current pinned path. A merge
alone does not update Aiken's vendored schemas, protocol reader or workspace.

| Upstream PR | Observed status and relevance | Current fallback |
| --- | --- | --- |
| [PlutusCoreBlaster #69](https://github.com/input-output-hk/PlutusCoreBlaster/pull/69) | Open: opt-in JSON verdict report. Review fixes and integration remain upstream work. | Exact pinned log/summary reader; Aiken emits its own JSON telemetry. |
| [PlutusCoreBlaster #70](https://github.com/input-output-hk/PlutusCoreBlaster/pull/70) | Draft: **Batch module hashing when capturing assurance environments**, stacked on `feat/assurance-counterexamples`. Plan §9's description as the typed-counterexample PR is not the current PR title/scope. | Keep current pins and log counterexamples. Reconcile report/counterexample APIs and coordinated pins deliberately before adopting a new consumer. |
| [PlutusCoreBlaster #74](https://github.com/input-output-hk/PlutusCoreBlaster/pull/74) | Open: ASCII ordering in assurance expressions. | Use Unicode `≤` / `≥`. |
| [CardanoLedgerApiBlaster #14](https://github.com/input-output-hk/CardanoLedgerApiBlaster/pull/14) | Draft: upstream Auction assurance, including the `feat/ual-property-helpers` work. | Keep ledger commit `997b9ca`, which supplies `toLedgerData`; do not substitute moving `main`. |
| [UAL #3](https://github.com/input-output-hk/UniversalAnnotationLanguage/pull/3) | Open: independent purpose-isolation **and acceptance** templates, broader than the plan's original purpose-isolation wording. | Write complete properties by hand; no Aiken template syntax. |

The separate exhaustion-verdict request (UAL-21) and ordinary existential-binder
syntax report are [ready for user filing](e2e-results.md#upstream-reports--to-be-filed-by-the-user).
No filed issue or upstream fix is claimed. Bracketed binders work at the current
pin, and bounded falsification is the expected contract, not a current blocker.

UAL-15 recorded upstream default-branch drift, including removal of the core's
workspace lock at its observed head. Its [drift tool](../../scripts/ual/README.md#upstream-drift-ual-15)
reports differences and PR transitions without automatically re-pinning or
executing fetched candidate code. Pinned e2e success does not establish candidate
head compatibility. Scheduling the local job is external to this repository.

## Maintainer handoff

This document is the source for the plan PR description. Updating the plan branch
from `main` and changing the single PR from draft to ready-for-review are
maintainer actions; this ticket does neither. Upstream PR coordination and issue
filing remain maintainer/user actions. Site documentation follows after merge
in a separate repository; this PR's user documentation is in-tree.
