# Aiken UAL user guide

This guide describes the implemented producer and check driver as of **2026-11-09**, using the committed consumer pins. It is not a promise about a newer upstream checkout. Start with the [summary](summary.md) for scope and documentation-run validation, the [plan](plan.md) for design history, and the [recorded end-to-end results](e2e-results.md) for observed verdicts. A successful example run does not establish a result for your own properties or environment.

Aiken compiles annotated targets into UAL documents. PlutusCoreBlaster consumes those documents through Lean and Blaster SMT. Ordinary `aiken build`, `aiken check`, and `aiken export` do not require Lean. Generation alone is not verification; environment capture is not verification either. See [ADR-0001](adr/0001-aiken-is-a-ual-producer-only.md).

## 1. Write explicit annotations

### Comment blocks and properties

Put assurance blocks at module top level:

```aiken
//@ PREDICATE
def larger (old proposed : Int) : Prop := old < proposed
@//

//@ PROPERTY outbids for outbids
  "A larger proposed bid returns true."
  ∀ (old proposed : Int), larger old proposed → fixtures__outbids_returns (Data.I old) (Data.I proposed) true
@//
```

The delimiters are `//@` and `@//`. The closing delimiter must be at line start or preceded by whitespace. Nested openers and unterminated blocks are lexer errors; formatting preserves the annotation text rather than formatting its Lean interior. The first word is exactly uppercase `PREDICATE` or `PROPERTY`, without aliases. The syntax and rationale are in [ADR-0002](adr/0002-annotations-in-comments.md); header parsing is implemented in [`parser.rs`](../../crates/aiken-project/src/assurance/parser.rs).

A property has this shape:

```text
PROPERTY <id> for <target-or-target.handler> ... "<description>" <Lean proposition>
```

- IDs match `[A-Za-z0-9_-]+` and are unique across the project.
- At least one scope, a nonempty quoted description, and a nonempty proposition are required. Multiple scopes are separated by whitespace, not commas.
- A scope names a function or validator **defined in the property's own module**, not an imported or module-qualified target. Every scoped target, including a validator, must have `@onchain`.
- A function scope cannot have a handler suffix. A validator suffix names a handler it defines, or `else` (including its implicit fallback).
- All targets in one property must have identical step budgets and semantics variants.
- The proposition is an explicit specification, not a translation of an Aiken `test` or a fuzzer domain. There is no fuzzer-to-quantifier conversion ([ADR-0005](adr/0005-no-fuzzer-to-quantifier-lift.md)).

`PREDICATE` bodies provide Lean helper definitions. The pinned consumer accepts a restricted definitional expression profile, including ordinary `def`/`abbrev`, not tactics, attributes, or `by` proofs. Aiken does not elaborate Lean: passing producer validation does not guarantee that the consumer accepts or proves the proposition. At this pin, use Unicode `≤`/`≥`, not ASCII `<=`/`>=` in Lean annotations. This does not change Aiken expression syntax outside annotations.

All predicate blocks in a module are concatenated in source order into one fragment. Aiken `use` edges provide fragment dependencies, traversing modules without fragments, so an annotation-free bridge module still carries its imports. Annotation-only modules with `use` declarations are supported.

#### Helper names

A helper `larger` defined in module `proofs/helpers` (file `lib/proofs/helpers.ak`) has four related names:

| Concept | Rule | Example |
| --- | --- | --- |
| Aiken module | Path below `lib/` or `validators/` without `.ak`; `-` becomes `_` | `proofs/helpers`, `example`, `a.b` |
| UAL fragment id | `/` becomes `.` and `.` becomes `-`; a bundle key, never a Lean name | `proofs.helpers`, `example`, `a-b` |
| Lean namespace | Module segments split at `/`; a segment is escaped `«…»` when it is not a plain Lean identifier, including Lean keywords | `proofs.helpers`, `«example»`, `«a.b»` |
| Helper reference | Namespace, `.`, then the helper name as you declared it | `proofs.helpers.larger`, `«example».positive` |

Write imported helpers by their qualified name, with the full module path. `use proofs/helpers as h` does **not** make `h.larger` a Lean name. In the property's own module, refer to helpers by the names you declared, as before. Each fragment emits your declarations unchanged and then a module-qualified copy of each one, so both spellings work in every property whose fragment closure contains the module. The pinned consumer accepts only `def`/`abbrev` commands, so the copies are full definitions, not `namespace` blocks or aliases. Implicit parameters such as `{α : Type}` work in both spellings.

Within one property's fragment closure:

- Every emitted name, declared or qualified, must name exactly one helper. Two imported modules both defining `larger` are rejected, as is module `a`'s helper `b.f` next to module `a/b`'s `f`: both are `a.b.f`. The error names both definitions.
- A qualified copy is elaborated with its module namespace open. If that would make a name inside it resolve to a different helper than in your declaration, the producer rejects the closure instead of letting the copies disagree.
- A reference that spells a project helper's qualified name exactly, such as `helpers.valid`, is rejected when module `helpers` is not in the property's import closure: import the module. A name that only starts with a module's namespace is not treated as a module reference. `ctx.scriptContextTxInfo` on a bound `ctx` is a projection, even if the project has an unrelated `lib/ctx.ak`. Unqualified, unknown or misspelled names are left to the consumer's elaboration error. This lexical scan does not replace consumer elaboration.

Fragment ids are injective over Aiken module names, so `a/b.ak` and `a.b.ak` keep separate fragments (`a.b`, `a-b`) and namespaces (`a.b`, `«a.b»`). Recursive helpers are rejected by the pinned consumer's fragment audit, whatever their spelling. See [`mod.rs`](../../crates/aiken-project/src/assurance/mod.rs) and [`lean.rs`](../../crates/aiken-project/src/assurance/lean.rs).

### Execution settings: no implicit defaults

```aiken
@onchain(steps: 100, semantics: e)
pub fn outbids(old: Int, proposed: Int) -> Bool {
  proposed > old
}
```

`@onchain` is allowed only on top-level functions and validators, not tests, handlers, or data types. Specify **both** settings:

| Setting | Implemented meaning |
| --- | --- |
| `steps` | Positive integer literal, from 1 through 18446744073709551615; a CEK-step bound, not a ledger ex-unit budget or wall-clock timeout. |
| `semantics` | `c` or `e`, case-insensitive; normalized to `C` or `E`. These are the supported V3 semantics variants; E is post-Conway. |

Missing, duplicate, or unknown settings and duplicate decorators are rejected. There is no default budget, default semantics, or automatic choice in the missing-decorator fix-it. The example's 100 steps is a fixture setting, not a recommendation for every program. Function boundaries must satisfy the exportable ABI restrictions: opaque types, remaining free type variables, and function-typed boundaries are rejected, including unsupported nested boundary types. See [`tipo/infer.rs`](../../crates/aiken-lang/src/tipo/infer.rs), especially decorator and onchain-interface validation.

Choose the semantics variant for the execution environment you intend to model,
not whichever variant makes a claim pass. Choose a step bound large enough for
the executions your property intends to cover, then inspect the verdict and
counterexamples. A too-small bound can falsify an exact-result claim even when
more steps would yield the expected result (§6). Increasing it requires a fresh
check and can increase consumer work; it does not turn the result into an
unbounded guarantee. Increasing `--timeout` only gives the consumer more wall
time and does not increase the CEK bound.

Aiken derives the ONCHAIN interface and reachable type definitions; do not write `ONCHAIN` or `UPLC_DATA` comment blocks. Function arguments are passed as Data (the example constructs `Data.I` explicitly), with native results for primitive types and native lists/pairs containing Data elements; ADT results remain Data. Validator parameters use their derived parameter interface and the runtime context is raw Data. Inspect the generated interfaces rather than guessing encodings. See [ADR-0003](adr/0003-derived-onchain-signature.md), [`function.rs`](../../crates/aiken-project/src/assurance/function.rs), and [plan §3.1](plan.md#31-derived-onchain-signature).

### Source names versus emitted wrapper names

Scopes use Aiken names such as `outbids` or `vesting.spend`. Propositions call emitted wrapper names:

```text
module/path.target → module_path__target
```

The producer replaces `/`, `-` and `.` in the module path with `_`, appends `__`, then the declaration name. Thus the `outbids` function in `validators/fixtures.ak` is `fixtures__outbids`; its exact-result relation is `fixtures__outbids_returns`. A dotted file name works too: validator `vesting` in `validators/fixtures.extra.ak` is `fixtures_extra__vesting`, while its blueprint title stays `fixtures.extra.vesting`. `<id>` exposes the evaluation state and `<id>_returns` expresses an exact result within the bound. Mapping collisions are errors, not silently renamed targets. See [ADR-0007](adr/0007-emitted-target-ids.md) and `emitted_target_id` in [`mod.rs`](../../crates/aiken-project/src/assurance/mod.rs).

The consumer declares more names than the ids, all in one Lean namespace per property: `<id>_script` and `<id>_returns` for each scoped function, and for every validator its title binding (`fixtures_extra_vesting`, every non-alphanumeric character replaced by `_`), `<title>_hash`, `<title>_paramCount` when it has unapplied parameters, and `_script` names for the property's checking target. If one property would declare a name twice, generation fails and names both sources. Common cases:

- functions `f` and `f_returns` (or `f_script`) scoped by the same property;
- validators `v` and `v_hash` in one module, which collide in every property;
- validator `v` in modules `m` and `m_`, or in `a.b` and `a_b`;
- a predicate helper named like any of these.

Scoping `f` and `f_returns` in separate properties is fine. The full inventory is in [ADR-0007](adr/0007-emitted-target-ids.md#generated-name-collisions).

## 2. Validator purposes are premises

The target is the **whole compiled validator**, including dispatch and fallback, never a separately compiled handler. Its compiled interface exposes all six V3 invocations. Selecting a checking-context `purpose` picks an invocation convention; it does **not** restrict the supplied V3 context to that purpose. The proposition's premise supplies that restriction ([ADR-0004](adr/0004-validator-is-the-target-purposes-are-premises.md)).

This is the committed [universal-validator fixture](../../crates/aiken/tests/fixtures/ual/positive/universal_validator/validators/fixtures.ak):

```aiken
@onchain(steps: 2500, semantics: e)
validator vesting(minimum: Int) {
  spend(_datum: Option<Data>, _redeemer: Data, _ref: Data, _tx: Data) {
    minimum > 0
  }

  else(_ctx: Data) {
    fail
  }
}

//@ PROPERTY positive_minimum for vesting.spend
  "A nonpositive minimum rejects spending."
  ∀ (minimum : Int) (ctx : ScriptContext),
    minimum ≤ 0 → isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx))
@//

//@ PROPERTY fallback_rejects for vesting.else
  "All non-spending invocations fail."
  ∀ (minimum : Int) (ctx : ScriptContext),
    isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx))
@//
```

For the spend property, Aiken inserts this premise after the outer universal binders and before the original body:

```lean
(∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d) →
  (minimum ≤ 0 → isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx)))
```

The complete lowered proposition, not just the author's original text, appears in `assurance.json`.

| Scope suffix | Positive premise on `ctx.scriptContextScriptInfo` |
| --- | --- |
| `spend` | `∃ (ref : _) (d : _), … = SpendingScript ref d` |
| `mint` | `∃ (cs : _), … = MintingScript cs` |
| `withdraw` | `∃ (cred : _), … = RewardingScript cred` |
| `publish` | `∃ (i : _) (cert : _), … = CertifyingScript i cert` |
| `vote` | `∃ (voter : _), … = VotingScript voter` |
| `propose` | `∃ (i : _) (pp : _), … = ProposingScript i pp` |

`else` adds the conjunction of the negations of the validator's defined handlers, or `True` if it defines none. An unsuffixed validator scope adds **no purpose premise**. `else` and unsuffixed scopes select `spend` in the checking context as the canonical V3 convention; that is not a claim that their input is spending. Other suffixed scopes select the corresponding purpose.

**Context binder requirement:** handler-scoped properties must start with explicit outer `∀`/`forall` binders containing exactly one binder named `ctx`. Prefer `∀ (parameters : Type) (ctx : ScriptContext), ...`; chained universal binders also work. A context called `context`, an inner existential context, or a `ctx` merely mentioned in a type/body does not meet this requirement. Unsupported binder forms must be rewritten; Aiken uses a small prefix recognizer, not a Lean parser. Unsuffixed scopes do not require this lowering-specific binder. Consumer elaboration still checks the actual types.

**Several scopes in one property** conjoin their premises against the **same** `ctx`, so each scope narrows the purposes the property is about:

| Scope | Purposes it admits |
|---|---|
| `v` (no suffix), or a function | all six |
| `v.spend`, `v.mint`, … | that one purpose |
| `v.else` | every purpose `v` has no explicit handler for |

A property must leave at least one purpose that every scope admits. Otherwise no script context could satisfy its premises, and the claim would "verify" vacuously. `aiken verify generate` (and both dry runs) rejects such a property, names the conflicting scopes, and writes nothing. For example, `for vesting.spend vesting.mint` is rejected, as is `for vesting.spend vesting.else` when `vesting` has a `spend` handler. Scopes on different validators are judged by each validator's own handlers: `for vesting.spend minter.else` is accepted, because `minter.else` excludes only mint. To claim something for several purposes, write a separate property for each. To state an alternative ("spend or mint"), use an unsuffixed scope (`for vesting`) and write the purpose condition in the proposition yourself.

This check is exact for the purpose premises Aiken inserts, and only for those. Aiken does not check whether your own antecedents can be satisfied: `∀ ctx, False → P` is still your responsibility. The implementation is [`purpose.rs`](../../crates/aiken-project/src/assurance/purpose.rs). The bracketed existential syntax above is deliberate: the pinned consumer rejected unbracketed existentials in the [recorded experiment](e2e-results.md#producer-discrepancies-and-purpose-comparison).

### Applied deployments

Bind every validator parameter by appending `[CBOR_HEX,...]` to that property's scope:

```aiken
//@ PROPERTY deployed_rejects for vesting.spend[00]
  "The zero-minimum deployment rejects spending."
  ∀ (ctx : ScriptContext),
    isUnsuccessful (fixtures__vesting (toLedgerData ctx))
@//
```

Values use the same hex-encoded Plutus Data CBOR as `aiken blueprint apply`: `00` is integer zero, `182a` is integer 42, and `41ab` is a one-byte byte array. They must cover **all** validator parameters in declaration order and match their blueprint schemas. The `[` immediately follows the scope; whitespace inside the list is allowed. Function scopes, empty/partial applications, extra parameters, malformed/trailing CBOR, and conflicting bindings on repeated scopes are rejected.

The applied wrapper takes **runtime arguments only**: omit parameter arguments from its call. Aiken preserves the proposition as written and still adds handler premises; it does not remove parameter quantifiers or infer a deployed theorem from a universal one. Without brackets, that property's scope remains universal. Independent properties can bind different deployments of the same validator in one bundle.

Generation retains the template blueprint and emits raw Flat value terms, a single-CBOR specialized program, its script hash and a `mode: applied` checking context. Changing a value changes the specialized script and context hashes, not the template. See [ADR-0008](adr/0008-property-local-applied-parameters.md) and the [applied Fixture 2 variant](../../crates/aiken/tests/fixtures/ual/positive/applied_validator/validators/fixtures.ak).

## 3. Provision the consumer and capture its environment

Use an Aiken executable built from this checkout with `verify` support. The consumer workspace setup requires Bash, Python 3.11+, Git, access to the public pinned repositories, installed Lean/Lake 4.24.0, and an assurance Z3 with the `recfun-finder` simplifier. The supported recorded run used Z3 5.1.0 at the source baseline below; stock Z3 4.15.2 is insufficient. Setup checks the solver capability and records its identity, rather than enforcing an exact Z3 binary or source baseline. Aiken itself does not install or repair this workspace or download toolchains.

The authoritative pins are [`schemas/PIN`](../../crates/aiken-ual/schemas/PIN) and the vendored [`assurance-workspace.json`](../../crates/aiken-ual/schemas/assurance-workspace.json):

| Component | Committed pin |
| --- | --- |
| PlutusCoreBlaster | `1c23de8f35fdc5decf6f126c7b56ed6649e4a57f` |
| Lean-blaster | `bc03bb402ce702984875bbf8ba51216bd6f843d7` |
| CardanoLedgerApiBlaster | `997b9caafb3f72f45d866929c2f561945d036e10` |
| UAL | `1f7ab847736e5fa53cadc2099300cdaf3c720c6b` |
| Lean | `4.24.0` |
| Z3 source baseline | `ca39e58c525c5b03c2484f118ade2b92fae31215` |

From the Aiken repository root, with the installed assurance Z3 on `PATH`:

```sh
export PATH="${ELAN_HOME:-$HOME/.elan}/toolchains/leanprover--lean4---v4.24.0/bin:$PATH"
scripts/ual/workspace-setup.sh --workspace "$HOME/ual-workspace"
export AIKEN_ASSURANCE_WORKSPACE="$HOME/ual-workspace/CardanoLedgerApiBlaster"
```

The setup script provisions sibling `PlutusCoreBlaster`, `Lean-blaster`, and `CardanoLedgerApiBlaster` checkouts and checks their pinned revisions and cleanliness. Use one setup process per workspace. Its `--workspace` option overrides `UAL_WORKSPACE`; otherwise the sibling root defaults to `$HOME/ual-workspace`. **`UAL_WORKSPACE` is not the CLI setting.** The CLI needs the **ledger checkout**, not the sibling root or `PlutusCoreBlaster`, because its Lake environment exposes both required imports.

Alternatively configure your Aiken project:

```toml
[assurance]
workspace = "../ual-workspace/CardanoLedgerApiBlaster"
```

`[assurance].workspace` takes precedence over `AIKEN_ASSURANCE_WORKSPACE`. Relative TOML paths resolve against `aiken.toml`; relative environment-variable paths resolve against the invocation directory. An explicitly configured invalid path does not silently fall back.

Capture the environment from the project directory (or outside a project with the environment variable set):

```sh
aiken verify env --out environment.json
```

`--out` defaults to `environment.json`; relative paths resolve against the invocation directory and the parent directory must exist. Capture runs `lake env lean` with imports `PlutusCore.UPLC.BlueprintEncoding.Assurance` and `CardanoLedgerApi.V3`. Keep the resulting manifest's exact bytes; generation hashes them without normalization. Keep generated inputs outside the clean consumer checkouts.

`environment.json.run.json` records revisions, warnings, capture status, and the manifest hash on success. Revision mismatches **warn and continue**; they are not a hard pin-enforcement gate. A failed capture does not label an old manifest as newly captured. The manifest is replaced only after successful invocation and validation. See [`consumer.rs`](../../crates/aiken-project/src/assurance/consumer.rs), the [`env` CLI](../../crates/aiken/src/cmd/verify/env.rs), and the detailed [workspace instructions](../../scripts/ual/README.md).

## 4. Run a committed example

The following copies only the source and package manifest from the [native-function fixture](../../crates/aiken/tests/fixtures/ual/positive/native_function/validators/fixtures.ak), leaving committed fixture files untouched. Run from the Aiken repository root after setup, with `aiken` on `PATH`:

```sh
example="$(mktemp -d)"
fixture="crates/aiken/tests/fixtures/ual/positive/native_function"
mkdir "$example/validators"
cp "$fixture/aiken.toml" "$example/aiken.toml"
cp "$fixture/validators/fixtures.ak" "$example/validators/fixtures.ak"
cd "$example"

aiken verify env --out environment.json
aiken verify generate --no-environment
aiken verify generate --environment environment.json
aiken verify check --environment environment.json --timeout 300
```

The fixture is self-contained and has no package dependencies. In a project with missing/stale dependencies, run `aiken build` before a dry run: dry runs use installed dependencies only. Do **not** copy `golden/environment.json` into a real check; golden manifests are synthetic producer-test inputs, not your loaded consumer environment.

The source includes the predicate/property shown in §1, the decorated `outbids` function, and this unscoped validator:

```aiken
validator companion {
  else(_ctx: Data) {
    fail
  }
}
```

Keep it in this example: the pinned blueprint schema requires a real validator even for this function-only property. The `companion` is not covered by the `outbids` property. The [recorded 2026-11-09 run](e2e-results.md#final-run--2026-11-09-bounded-semantics-decision) verified `outbids` at 100 steps/E and published evidence. Your own command must obtain a fresh successful result; an old recorded verdict is not evidence for changed inputs.

## 5. Commands, files, and failure behavior

| Command | Inputs and behavior |
| --- | --- |
| `aiken verify generate [DIRECTORY] --environment PATH` | Compile, validate, bind the manifest's original bytes, and publish the bundle. Does not run Lean or a solver. |
| `aiken verify generate [DIRECTORY] --no-environment` | Validate an unbound draft; write nothing. Conflicts with `--environment`. |
| `aiken verify generate [DIRECTORY] --environment PATH --dry-run` | Also validate the manifest and final bindings, but write no bundle, lock, or cache files. |
| `aiken verify check [DIRECTORY] --environment PATH --timeout 300` | Generate afresh, invoke the consumer, require every property verified, and publish evidence only on complete success. The manifest defaults to `environment.json`; timeout defaults to 300 seconds and must be positive. |
| `aiken verify check --show-json-schema` | Print the non-TTY report schema without loading a project or consumer workspace. |
| `aiken verify env --out PATH` | Capture the loaded environment; no positional project-directory argument. |

Generate requires `--environment` unless `--no-environment` is supplied; it does not implicitly use `environment.json`. All manifest paths are relative to the invocation directory, including when a positional project directory is supplied. Generate and check support Aiken workspaces and process their member projects; check fails if any member fails and rejects an empty workspace.

Generate and check also accept `--env NAME` (the Aiken configuration environment, **not** the consumer manifest), `--trace-level silent|compact|verbose` (default `silent`), and `--trace-filter user-defined|compiler-generated|all` (`-f`; filter default is all). These affect compilation, so retain the same settings when comparing bundles. See the exact CLI definitions: [`generate.rs`](../../crates/aiken/src/cmd/verify/generate.rs), [`check.rs`](../../crates/aiken/src/cmd/verify/check.rs).

Generated artifacts:

| Path | Role |
| --- | --- |
| `assurance/plutus.json` | Compiled-interface blueprint, separate from ordinary root `plutus.json`. |
| `assurance/assurance.json` | Properties, complete purpose-lowered propositions, fragments, and function registry. No verification evidence from generation. |
| `assurance/checking-<property-id>.json` | One per property; target bindings, declared semantics and CEK budget, `acceptance: evaluation`, universal or applied parameter mode, and environment binding. |
| `assurance/applied-value-<sha256>.flat` | Applied parameter as a closed Data constant, raw Flat term without a program-version prefix. |
| `assurance/applied-script-<sha256>.cbor` | Specialized program: exact ordered application of the template, single-CBOR Flat. |
| `assurance/Check.lean` | Check driver, with the capture imports and opened V3/ScriptInfo namespaces. |
| `assurance/verification.log` | Full consumer log once the process has been invoked, including failed verdicts/diagnostics. Early failures need not produce a log. |
| `assurance/run-report.json` | Successful-run trust/report artifact. |
| `assurance/verification-artifact.zip` | Successful-run exact inputs, environment, driver, log, and trust report. |
| `assurance/verified-assurance.json` | Published last on success, with `smt-check` evidence bound to each `checkingContextHash` and scoped target hashes. |

Generation removes stale checking-context files and content-addressed applied artifacts and never overwrites the root blueprint ([ADR-0006](adr/0006-dialect-blueprint-under-assurance.md)). Bundle references resolve relative to `assurance/`; retain every referenced artifact and the environment when moving a generated bundle. Function digests are SHA-256 of hex-decoded `compiledCode` (single CBOR-wrapped Flat); validator bindings use the script hash. Modified bytes are not interchangeable with verified bytes.

Before compilation or workspace loading, each check attempt removes stale evidence, its archive/report, old driver, and old log. It exits nonzero for no properties, any non-verified property, timeout, unsuccessful Lean exit, incomplete/unrecognized/duplicate/unknown verdicts, an inconsistent summary, or evidence-publication failure. On Unix the timeout terminates the consumer process group. A wall-clock timeout is separate from the declared CEK bound. Successful verdict text alone is not sufficient for success.

The pinned consumer has no JSON verdict-report option. The driver parses its exact text protocol and summary; changing the vendored consumer pin requires deliberately updating the registered reader. Installed revision mismatches themselves are warnings, not automatic rejection, so inspect the trust report and keep the supported pins. See [`check.rs`](../../crates/aiken-project/src/assurance/check.rs), especially `PinnedVerdictReader`, `clear_previous_run`, and `publish_evidence`.

TTY output shows the property table and diagnostics; non-TTY stdout emits a JSON report, with compiler diagnostics and the raw consumer log on stderr. Workspace checks emit one report per member. Consumers of this output should use `--show-json-schema`, not scrape the styled table.

The pinned evidence format has one `scriptHash` per validator-scoped property. A property spanning distinct template validator hashes cannot receive valid evidence and publication fails; split it into separate properties. This is a format limitation, not permission to omit a target binding. For applied properties, this field retains the template hash expected by the pinned consumer; `checkingContextHash` additionally binds the parameter terms and specialized script hash. The verification archive includes those binary artifacts.

## 6. Interpret results within their trust boundary

A successful result is **Blaster SMT verification, with no reconstructed Lean proof**. It relies on the compiled bytes, producer/consumer translation, the loaded PlutusCoreBlaster/CardanoLedgerApi/Blaster environment, and the actual Z3 executable. Reports include installed revisions and the inspected Z3 path/hash, and state that results are relative to the checking context's CEK-step budget. These are not ledger ex-unit guarantees, unbounded termination proofs, or a claim that the full upstream development is free of assumptions; the recorded run also notes an existing upstream `sorry` warning.

For an exact-result `_returns` claim, **falsified means a counterexample has no matching result within the declared bound**. At the pinned consumer, the relation is false for any state other than a matching `Halt`. It does not distinguish an unfinished evaluation from a completed wrong result and does not establish failure with unlimited steps. Inconclusive is the solver's `Undetermined` outcome, not a dedicated budget-exhaustion verdict.

The committed one-step case therefore correctly expects `outbids` to be **falsified**, a nonzero CLI exit, and **no evidence**. The [recorded counterexample](e2e-results.md) was `old = 0`, `proposed = 1`; the same function's 100-step claim verified. Changing a budget changes the proposition's execution conditions and requires a new check. UAL-21 tracks a separate upstream exhaustion-verdict request; it is not a prerequisite for accepting the current bounded semantics.

A verified helper property does **not** verify a validator that calls or inlines that helper. There is no helper-to-inlined-code equivalence guarantee. Check warns when a scoped helper is used by an uncovered validator, following transitive calls and imported aliases. Adding a validator-scoped property removes that advisory warning, but does not enlarge the helper property's claim or establish that the new property captures all desired validator behavior. See [`scope.rs`](../../crates/aiken-project/src/assurance/scope.rs). State and verify the validator claim explicitly.

## 7. Upstream PRs: dated status, not automatic capability upgrades

[Plan §9](plan.md#9-upstream-facts-resolved-2026-10-30) records the historical **2026-11-03** view. The public PR pages were checked for this documentation work on **2026-11-09**; the snapshot below is dated, not a promise of their current state when you read this. None changes this checkout's committed pins or installed CLI capabilities.

| PR | Public status on 2026-11-09 | What to do at the committed pin |
| --- | --- | --- |
| [PlutusCoreBlaster #69](https://github.com/input-output-hk/PlutusCoreBlaster/pull/69) | Open; opt-in JSON verification report. | Continue using the pinned log reader. Do not add `(report := ...)` to the current driver. |
| [PlutusCoreBlaster #70](https://github.com/input-output-hk/PlutusCoreBlaster/pull/70) | Draft, **“Batch module hashing when capturing assurance environments”**, stacked on `feat/assurance-counterexamples`. | Keep the current coordinated pins. The plan's older description as the typed-counterexample PR is not an accurate description of this PR itself; do not switch to its branch based on that table. |
| [PlutusCoreBlaster #74](https://github.com/input-output-hk/PlutusCoreBlaster/pull/74) | Open; ASCII ordering syntax. | Use `≤`/`≥` in annotation Lean. |
| [CardanoLedgerApiBlaster #14](https://github.com/input-output-hk/CardanoLedgerApiBlaster/pull/14) | Draft, **“Verify the upstream Auction audit through generated assurance”**, on the helper branch. | Use immutable ledger pin `997b9ca…` supplying the required helpers, not a moving `main` or branch name. |
| [UAL #3](https://github.com/input-output-hk/UniversalAnnotationLanguage/pull/3) | Open, **“Specify independent purpose-isolation and acceptance templates”**; broader than the plan's purpose-only description. | Write explicit properties and premises; no Aiken template feature is implied. |

For decisions and drift notes see the [decision log](decisions-log.md). Re-pinning requires checking the coordinated workspace, accepted proposition syntax, verdict protocol, schemas, and bounded-result behavior; an upstream merge alone does not update any of them here.
