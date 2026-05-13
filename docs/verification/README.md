# Verification result guide

This guide explains how to read `aiken verify` results without opening generated Lean files.

Start with:

- `aiken verify doctor`
- `aiken verify capabilities`
- `aiken verify run --json --artifacts always`

Use `doctor` to confirm the local toolchain, `capabilities` to see what this build can honestly report, and `--json --artifacts always` to keep the generated manifest, Lean files, logs, and any SMT-LIB artifacts referenced by the result.

Related documents:

- [Developer guide](./developer-guide.md)
- [Schema migration guide](./schema-migration.md)
- [Golden fixtures](./fixtures/README.md)

## 1. Read `status` first

Every theorem result has a canonical `status`.

| Status | Meaning | What it does **not** mean |
| --- | --- | --- |
| `LeanCertified` | Lean's kernel checked a reconstructed proof. | Not every successful proof reaches this level. |
| `SolverValidated` | Blaster/Z3 validated the generated theorem. | This is not a Lean-kernel certificate. |
| `SolverFalsified` | The solver found a counterexample to the generated theorem. | For approximate domains, that model is not automatically a real generated Aiken test input. Check replay. |
| `WitnessValidated` | A concrete replayed/generated witness was validated on the intended path. | This is not a universal proof. |
| `Partial` | The verifier produced a sound artifact, but one or more obligations remain open. | Not production-success unless your process explicitly accepts partial results. |
| `Unknown` | The verifier did not prove or refute the theorem truthfully. | Not failure, but also not verification. |
| `TimedOut` | Verification hit the configured timeout or fuel boundary. | Not proof and not refutation. |
| `Unsupported` | The current integration could not generate or run a sound verification problem. | Not an unknown proof outcome; this is an integration/support limit. |

Current preview builds normally report `SolverValidated`, not `LeanCertified`, for successful universal proofs. Check `aiken verify capabilities` on your binary before treating `LeanCertified` as available.

## 2. Then read `certification`

`certification` tells you why the status was reported.

| Certification | Meaning |
| --- | --- |
| `lean_kernel_checked` | Proof reconstruction reached Lean's kernel. |
| `smt_valid_no_proof_reconstruction` | The solver validated the theorem, but there is no Lean-kernel reconstruction. |
| `witness_replay` | Concrete replay/native evaluation supplied the evidence. |
| `open_obligations` | The result is intentionally incomplete. |
| `timeout` | Timed out. |
| `unsupported` | Unsupported theorem/integration path. |
| `unknown` | No stronger evidence class is available. |

A result is strongest when `status`, `certification`, `trust_profile`, and `domain` all agree. For example, `SolverValidated` plus `smt_valid_no_proof_reconstruction` is honest. `SolverValidated` does **not** imply kernel certification.

## 3. Trust profiles

The selected `trust_profile` controls which successful-looking results count as acceptable.

| Trust profile | Accepts |
| --- | --- |
| `strict` | Only `LeanCertified`, plus obligations/certificates acceptable under strict semantics. |
| `production` | `LeanCertified` and selected `SolverValidated` / `WitnessValidated` results with trusted certificates. |
| `experimental` | May accept weaker evidence such as `DifferentialTestedOnly`, but must label it clearly. |
| `unsafe-dev` | Can run experiments and placeholder paths, but must not print a production verification claim. |

<!-- BLASTER_REVIEW_RISK(ci_guidance_missing_solver_profile): `strict-cert` is selected via `run_settings.solver_profile`, so CI consumers that record only trust_profile + status cannot fully reconstruct acceptance policy. -->
If you are wiring results into CI, record both `run_settings.trust_profile` and `status`. A `SolverValidated` result can still be rejected by `strict-cert` or by an incompatible trust profile.

## 4. Domain precision and certificates

The exported domain is the contract between the theorem and the real Aiken fuzzer.

### Precision

| Precision | Safe for universal `normal` / `fail`? | Safe for semantic `fail_once`? |
| --- | --- | --- |
| `Exact` | Yes | Yes |
| `OverApprox` | Yes | No, not by itself |
| `UnderApprox` | No | Yes |
| `WitnessOnly` | Only as a concrete witness check | Yes |
| `Unknown` | No | No |

Interpretation:

- `Exact`: the domain matches the generated support.
- `OverApprox`: the domain contains every generated input, and maybe more.
- `UnderApprox`: the domain contains only generated inputs, but not necessarily all of them.
- `WitnessOnly`: the result is about specific replayed/generated values.
- `Unknown`: there is no trustworthy semantic relation yet.

### Certificates

| Certificate | Meaning |
| --- | --- |
| `LeanProved` | The domain relation itself was discharged in Lean. |
| `TrustedVersionedModel` | The relation comes from a pinned/hash-checked trusted model. |
| `SamplerSemanticModel` | The relation is stated directly in sampler/UPLC semantics. |
| `WitnessReplay` | Concrete replay validated the path. |
| `DifferentialTestedOnly` | Regression evidence only. Not production proof by itself. |
| `Unchecked` | Placeholder/unsupported/unjustified path. |

Practical rule:

- Universal success/failure needs a domain that safely covers generated inputs.
- Existential `fail_once` needs a domain that is generated by the real fuzzer, or a replay-backed witness.
- `Unknown` or `Unchecked` domains must not become production-success.

## 5. Obligations

`domain.obligations_open` and `domain.obligations_discharged` tell you exactly which claims still need evidence.

Common obligations:

- `FuzzerReturnsImpliesDomain`
- `DomainImpliesFuzzerReturns`
- `DomainIffFuzzerReturns`
- `WitnessReplaysThroughFuzzer`
- `WitnessSatisfiesDomain`
- `FuzzerModelHashMatches`
- `ValueDecoderRoundTrip`
- `FuzzerOutputTypeMatchesPropertyInputType`
- `PropertyHarnessAcceptsDecodedInput`
- `PropertyHarnessMatchesExportedUPLC`

Interpret them literally:

- Open obligations mean the system is still missing proof or trusted-model evidence.
- Discharged obligations say where the current trust rests.
- If a result is `Partial`, the open obligations are the first place to look.

## 6. Replay and witnesses

Replay shows whether the reported witness or counterexample ran through the intended Aiken path.

Look for:

- `domain.sampler.run_kind`
- `domain.sampler.generation_seed`
- `domain.sampler.replay_choices_hex`
- `counterexample.replay_status`
- `counterexample.property_outcome`

Important distinction:

- `WitnessValidated` means a concrete value or trace was replayed and checked.
- It does **not** mean the randomized runner is guaranteed to find that witness within some attempt bound.
- A bounded-runner guarantee needs a separate theorem that includes the seed/retry schedule.

## 7. Counterexamples

`SolverFalsified` results may include a `counterexample` block.

Read these fields carefully:

- `classification = confirmed_by_replay`: replay confirmed a real Aiken failing input.
- `classification = potential`: the solver found a model for an approximate domain, but replay did not yet confirm a real generated input.
- `classification = smt_model_only`: the raw model was kept, but decoding/replay was incomplete.

`replay_status` refines that story:

- `confirmed`
- `replay_failed`
- `decode_failed`
- `not_attempted`

Do not present `potential` or `smt_model_only` as a confirmed failing Aiken test case.

## 8. Artifacts and metrics

Top-level JSON includes `artifacts` whenever files were retained:

- `artifacts.manifest`
- `artifacts.lean_root`
- `artifacts.logs`
- `artifacts.smt2[]`

Use `--artifacts always` when you want stable paths for debugging or CI collection.

Other useful fields:

- `run_settings.solver_profile`
- `run_settings.trust_profile`
- `run_settings.timeout_secs`
- `run_settings.cek_fuel`
- `cache.*` for warm/cold rerun behavior
- `theorems[].metrics.wall_ms`
- `theorems[].metrics.formula_size`
- `theorems[].metrics.domain_complexity`

## 9. Reading a result without Lean

Recommended order:

1. Check `command_success` and top-level counts.
2. For each theorem, read `status` and `certification`.
3. Read `trust_profile`.
4. Read `domain.precision`, `domain.certificate`, `obligations_open`, and `widenings`.
5. If present, read `counterexample.classification` and `replay_status`.
6. Use `explanation` before opening generated Lean.
7. Open the retained artifacts only if the structured fields are not enough.

## 10. Golden fixtures

The committed fixtures under [`docs/verification/fixtures`](./fixtures/README.md) show stable examples for:

- primitive universal success
- `fail` with `Void`
- relational custom fuzzers
- `fail_once` witness replay
- sampler fallback
- bounded scenario partial results

They are intended to be readable examples of the current JSON contract, not promises that every project will produce the same proof strength.
