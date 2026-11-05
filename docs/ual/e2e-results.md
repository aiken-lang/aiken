# UAL-13 real-consumer results

## Status and reproduction

The pinned consumer does **not** satisfy the required exhaustion verdict. Reducing
fixture 1 to one CEK step reports `falsified`, not `inconclusive`. The runner keeps
`inconclusive` as its expectation and exits nonzero on this discrepancy. UAL-13's
all-green acceptance criterion is therefore blocked; this is not a successful
end-to-end signoff.

```sh
scripts/ual/e2e.sh
# Keep inputs, environment, generated drivers, JSON reports and raw logs:
scripts/ual/e2e.sh --output /tmp/ual-e2e-run
```

The runner provisions/verifies the locked consumer workspace, builds this Aiken
checkout, captures a real two-import environment, and copies fixture sources
without their synthetic goldens. Ordinary and negative cases use `aiken verify
check`. Tampered bundles use the generated driver with `lake env lean`, because
`verify check` would regenerate and overwrite the corruption. No upstream source,
consumer pin, solver, step-budget default, or CI configuration was changed.


## Final fixture run

Command: `scripts/ual/e2e.sh --output /tmp/ual-13-e2e-fixed`.
Observed exit status: **1**, with **17/18 cases passed**. Total wall time:
203.70 seconds, including environment capture and setup verification.

| Fixture/case | Expected | Observed | Check wall time |
| --- | --- | --- | ---: |
| 1: native function, 100 steps, E | verified | `outbids`: verified; evidence published | 14.801 s |
| 2: universal validator, 2500 steps, E | both verified | `fallback_rejects` and `positive_minimum`: verified; evidence published | 35.146 s |
| 3: deliberately false equal bid, 100 steps, E | falsified with counterexample | `equal_bid_outbids`: falsified; `old = 0`, `proposed = 0`; no evidence | 20.640 s |
| 4: recursive Data, 2500 steps, C | verified | `preserves_leaf`: verified; evidence published | 18.678 s |
| Match-based equivalent of fixture 2 | both verified | Both verified; evidence published | 50.705 s |
| 5: one-step native function | inconclusive | **falsified**, `old = 0`, `proposed = 1`; no evidence | 19.448 s |
| 5: unknown scope | producer rejection | Expected diagnostic, no bundle/evidence | 3.137 s |
| 5: wrong-module scope | producer rejection | Expected diagnostic, no bundle/evidence | 3.094 s |
| 5: missing fragment | producer rejection | Expected diagnostic, no bundle/evidence | 3.098 s |
| 5: opaque parameter | producer rejection | Expected diagnostic, no bundle/evidence | 3.088 s |
| 5: missing steps | producer rejection | Expected diagnostic, no bundle/evidence | 3.079 s |
| 5: invalid semantics | producer rejection | Expected diagnostic, no bundle/evidence | 3.021 s |
| 5: mapped ID collision | producer rejection | Expected diagnostic, no bundle/evidence | 3.042 s |
| 5: mixed step budgets | producer rejection | Expected diagnostic, no bundle/evidence | 3.063 s |
| 5: mixed semantics | producer rejection | Expected diagnostic, no bundle/evidence | 3.022 s |
| 5: unterminated annotation | producer rejection | Expected diagnostic, no bundle/evidence | 3.078 s |
| 5: altered function compiledCode | consumer rejection | `function compiledCode hash mismatch`, Lean exit 1 | 1.440 s |
| 5: mismatched function digest | consumer rejection | `function compiledCode hash mismatch`, Lean exit 1 | 1.139 s |

Every `verify check` case retained its ordinary root blueprint sentinel.
The script checked exact property sets/verdicts, CLI exit/success agreement,
evidence presence/absence, counterexample bindings, and absence of trust warnings.
The successful runs did not reuse golden environments or cached property verdicts.

## Revisions and tools

Observed on Linux aarch64, 2026-11-05:

| Component | Observed identity |
| --- | --- |
| PlutusCoreBlaster | `1c23de8f35fdc5decf6f126c7b56ed6649e4a57f` |
| Lean-blaster | `bc03bb402ce702984875bbf8ba51216bd6f843d7` |
| CardanoLedgerApiBlaster | `997b9caafb3f72f45d866929c2f561945d036e10` |
| UAL specification pin | `1f7ab847736e5fa53cadc2099300cdaf3c720c6b` |
| Lean | `4.24.0`, commit `797c613eb9b6d4ec95db23e3e00af9ac6657f24b`, Release |
| Lake | `5.0.0-src+797c613` |
| Z3 | `5.1.0`, build `ca39e58c525c5b03c2484f118ade2b92fae31215` |
| Z3 binary SHA-256 | `6a9c23e251e44bffab2f5e7fe6d63ce247a891062d35a5d7a4836694b8776aea` |
| Rust | `rustc 1.94.1 (e408947bf 2026-03-25)` |
| Python | `3.13.5`, GCC 14.2.0 |
| Aiken | `v1.1.24+ded4de3`, working tree including the UAL-13 changes |

The required Z3 `recfun-finder` capability probe returned `sat`. The captured
manifest SHA-256 was
`65bbdc2e64b4c2e51bb82762199b5a3927429615ed235c11036a93935f473536`.
The full public pinned workspace was built locally; its committed Game smoke
verified all three properties. Setup took 587.7 seconds. Its existing
`PlutusCore/UPLC/CekMachine.lean:309` warning about `sorry` remains upstream and
unchanged. All property results below are **Blaster SMT, without reconstructed
Lean proofs**, relative to the declared CEK step budgets.

## Producer discrepancies and purpose comparison

The first complete run passed 15/18 cases. Fixture 2 and its match-based variant
failed before SMT; the third failure was the exhaustion verdict.

1. The driver imported `CardanoLedgerApi.V3` but did not open its namespace.
   The match variant reported unknown `ScriptContext`, `toLedgerData`, and
   `SpendingScript`. The check driver now opens `CardanoLedgerApi.V3` and
   `CardanoLedgerApi.V3.Contexts.ScriptInfo`. This changes name resolution, not
   loaded modules or environment-capture imports.
2. The generated `∃ ref d, ...` was rejected by the consumer's syntax profile:
   `formal source syntax 'Lean.unbracketedExplicitBinders' is outside the
   definitional profile`. Parentheses alone (`∃ (ref) (d), ...`) are not valid
   Lean here (`expected ':'`). The accepted form is
   `∃ (ref : _) (d : _), ...`; the constructor application infers the types.
   All six generated purpose premises now use these bracketed binders.

With the same namespace opens, both complete spend/else propositions verified:

| Premise | Direct consumer wall time | Verdicts |
| --- | ---: | --- |
| `∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d` | 58.90 s | Both verified |
| `match ctx.scriptContextScriptInfo with \| SpendingScript _ _ => True \| _ => False` | 67.49 s | Both verified |

These single-run timings overlap other work and are not performance benchmarks.
There was no discharge advantage from changing to match, so the producer retains
existentials with compatible binders. The e2e script also exercises the match
alternative through `verify check`, using an unsuffixed validator scope and an
explicit equivalent premise (negated for `else`). Plan §3.3 documents the choice.

One bundle snapshot and fixture 2's golden assurance document were reviewed and
updated individually. Only the two property-source strings changed, to bracket
the existential binders. Compiled code, interfaces, blueprint bytes, checking
contexts, digests, descriptions, and property meanings did not change. No
snapshots were bulk-accepted and no protected generator/evaluator files changed.

## Consumer discrepancy reports

**Not filed:** GitHub REST issue-creation requests for both reports below failed
with `Temporary failure in name resolution` in the worker. No GitHub CLI, token,
or Git credential helper is configured. Public read-only GitHub access was
available, but that does not provide authenticated issue creation. No issue URL
or upstream fix is claimed. The reports below are ready to file in
[PlutusCoreBlaster](https://github.com/input-output-hk/PlutusCoreBlaster/issues).

### CEK exhaustion is reported as a falsified exact-result proposition

Reproducer: `tiny_budget` in `scripts/ual/e2e.sh`. Start from
`crates/aiken/tests/fixtures/ual/positive/native_function`, change only
`steps: 100` to `steps: 1`, then regenerate/check with the real environment.
Generation correctly rebinds the checking-context digest.

The function `proposed > old` has compiledCode
`500101002232337100026eb4008dd68011`, two Data arguments, and native `#boolean`
result. The proposition is:

```lean
∀ (old proposed : Int),
  larger old proposed →
    fixtures__outbids_returns (Data.I old) (Data.I proposed) true
```

Here `larger old proposed` is `old < proposed`. At 100 steps the property verifies.
At one step the consumer reports:

```text
Counterexample:
 - old: 0
 - proposed: 1
Property 'outbids': falsified
Assurance 'fixtures/ual_native_function': 1 checked, 0 not checked, 1 failed/inconclusive
```

Expected by UAL-13: `inconclusive` for unfinished evaluation, without presenting
it as a completed wrong result. Actual: `falsified`, nonzero CLI exit, no evidence.

Source explanation at the pin:

- [`CekMachine.lean`, `runStepsPreservingState`](https://github.com/input-output-hk/PlutusCoreBlaster/blob/1c23de8f35fdc5decf6f126c7b56ed6649e4a57f/PlutusCore/UPLC/CekMachine.lean#L238-L259)
  preserves an unfinished CEK state when fuel reaches zero.
- [`Basic.lean`, `emitAssuranceFunction`](https://github.com/input-output-hk/PlutusCoreBlaster/blob/1c23de8f35fdc5decf6f126c7b56ed6649e4a57f/PlutusCore/UPLC/BlueprintEncoding/Basic.lean#L1248-L1277)
  defines `_returns` as `False` for every state other than a matching `Halt`.
- [`Assurance.lean`](https://github.com/input-output-hk/PlutusCoreBlaster/blob/1c23de8f35fdc5decf6f126c7b56ed6649e4a57f/PlutusCore/UPLC/BlueprintEncoding/Assurance.lean#L980-L989)
  maps Blaster's `Falsified` directly to `falsified`; `inconclusive` represents
  `Undetermined`, not a separate CEK-exhaustion classification.

The reported falsification is consistent with that bounded proposition. It does
not meet the requested exhaustion contract. Aiken cannot safely infer completion
from the text verdict or relabel every small-budget counterexample. Required
upstream prerequisite: an exhaustion/verdict contract that also handles arbitrary
propositions, followed by an approved consumer pin implementing it. The script
keeps the required expectation; it has no skip or expected-failure override.

### Ordinary existential binder syntax is rejected

Reproducer: fixture 2's original purpose premise:

```lean
∃ ref d, ctx.scriptContextScriptInfo = SpendingScript ref d
```

The pin rejects `Lean.unbracketedExplicitBinders` before elaboration, although
[`profileSyntaxKinds`](https://github.com/input-output-hk/PlutusCoreBlaster/blob/1c23de8f35fdc5decf6f126c7b56ed6649e4a57f/PlutusCore/UPLC/BlueprintEncoding/Assurance.lean#L625-L704)
accepts the parent `term∃_,_` notation and bracketed binders. This is ordinary
definitional syntax, not a tactic. Upstream should consistently support the
binder node while retaining the elaboration audit. Aiken works with the existing
pin by generating the semantically identical bracketed form; no upstream patch
or whitelist weakening was applied locally.

## Repository validation

Passed after the producer fixes:

```text
cargo build --workspace --all-targets
cargo build --workspace
cargo fmt --all -- --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test --workspace
aiken-acceptance
cargo test -p aiken-project assurance::
cargo test -p aiken --test ual_conformance
python3 -B -m unittest discover -s scripts/ual -p 'test_*.py'
bash -n scripts/ual/e2e.sh
```

Formatting initially requested wrapping three updated assertions; `cargo fmt
--all` applied those changes and the final formatting check passed. Acceptance
used the bounded `aiken-acceptance` wrapper, not an unbounded `bash ci` invocation.
The separate real-consumer e2e command remains red solely for the exhaustion
verdict described above.
