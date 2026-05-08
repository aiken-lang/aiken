# Migration guide: draft result schema to current verification schema

This guide is for tools that consumed the earlier preview JSON.

Related documents:

- [User guide](./README.md)
- [Developer guide](./developer-guide.md)
- [Golden fixtures](./fixtures/README.md)

## 1. What changed

The current schema separates three concerns that the earlier draft mixed together:

1. **Result status**: `status`
2. **Evidence strength**: `certification`
3. **Legacy proof detail**: `proof_status`

It also makes the fuzzer/domain contract explicit via `domain`, `input_type`, `trust_profile`, `counterexample`, `metrics`, `artifacts`, `run_settings`, and `cache`.

## 2. The main migration

### Old draft shape

Older theorem results used the legacy detailed proof vocabulary directly in `status`, for example:

```json
{ "status": { "kind": "proved" } }
```

Other legacy variants included:

- `{"kind":"witness_proved", ...}`
- `{"kind":"partial", ...}`
- `{"kind":"failed", ...}`
- `{"kind":"timed_out", ...}`
- `{"kind":"unknown"}`

### Current shape

Current theorem results always use a canonical top-level `status` string and keep the old detailed value in `proof_status` for compatibility.

Example:

```json
{
  "status": "SolverValidated",
  "certification": "smt_valid_no_proof_reconstruction",
  "proof_status": { "kind": "proved" }
}
```

## 3. Status mapping

Map old draft values to the new canonical vocabulary as follows.

| Legacy `proof_status.kind` | Current `status` | Current `certification` |
| --- | --- | --- |
| `proved` | `SolverValidated` | `smt_valid_no_proof_reconstruction` |
| `witness_proved` | `WitnessValidated` | `witness_replay` |
| `partial` | `Partial` | `open_obligations` |
| `failed` with `category = counterexample` | `SolverFalsified` | `smt_valid_no_proof_reconstruction` |
| `failed` with `category = blaster_unsupported` | `Unsupported` | `unsupported` |
| `failed` with other categories | `Unknown` | `unknown` |
| `timed_out` | `TimedOut` | `timeout` |
| `unknown` | `Unknown` | `unknown` |

`LeanCertified` is reserved for kernel-checked proof reconstruction. A legacy `proved` result does **not** upgrade to `LeanCertified` automatically.

## 4. New fields consumers should read

### Per theorem

New or newly important fields:

- `status`
- `certification`
- `trust_profile`
- `domain`
- `input_type`
- `counterexample`
- `explanation`
- `metrics`
- `proof_status` (compatibility detail, still useful)

### Top level summary

New or newly important fields:

- `artifacts`
- `elapsed_ms`
- `run_settings`
- `cache`
- `command_success`
- `blaster_rev`
- `plutus_core_rev`
- `allow_vacuous_subgenerators`
- `two_phase_disabled`
- `verify_summary_version`

## 5. Generated manifest migration

Generated manifests now use schema version 2.

### Old draft manifests

Older manifests could omit:

- `schema_version`
- `return_mode`
- `test_mode`
- `on_test_failure`
- `execution.plutus_version`
- `execution.cek_budget.fuel`
- `execution.decode_policy`
- domain/certificate metadata
- current hash-bearing fields

### Current manifests

Current manifests include explicit mode/execution metadata and hash-bearing fields where available.

Important fields:

- `schema_version = 2`
- `return_mode`
- `test_mode`
- `on_test_failure`
- `execution.plutus_version`
- `execution.cek_budget.fuel`
- `execution.decode_policy`
- domain precision/certificate/obligations
- property/fuzzer/harness/model hashes

## 6. Compatibility behavior

The parser still accepts older results/manifests, but it does **not** silently invent proof-grade assumptions.

Conservative compatibility rules:

- old theorem results are normalized into the new `status` vocabulary
- old `status` objects still deserialize through `proof_status`
- older manifests missing mode/execution/domain fields are read, but may downgrade to `Partial` or `Unsupported` when the missing information matters to soundness
- legacy placeholders synthesize conservative `Unknown` / `Unchecked` domain metadata instead of pretending the domain was exact

Consumers should treat missing v2 manifest metadata as a compatibility limitation, not as safe proof evidence.

## 7. Suggested consumer updates

### If you only need pass/fail

Prefer:

1. top-level `command_success`
2. theorem `status`
3. theorem `certification`

Do **not** look only at legacy `proof_status.kind`.

### If you need trustworthy automation

Also require:

- acceptable `trust_profile`
- acceptable `domain.precision`
- acceptable `domain.certificate`
- no unacceptable `obligations_open`
- `counterexample.replay_status` for failing results

### If you show messages to users

Display `explanation` before surfacing raw Lean or SMT output. The new schema is designed so users can interpret the result without opening generated files.

## 8. Version markers to watch

Current observed schema/version markers in this repo:

- verify summary: `verify_summary_version = "3"`
- generated manifest: `schema_version = 2`, `version = "2.0.0"`
- capabilities JSON: `version = "4"`
- doctor JSON: `version = "2"`

If a future release bumps one of these, expect a contract change and review the docs fixtures.
