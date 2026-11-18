# Aiken × UAL / Blaster integration plan

Status: planning. Agreed 2026-10-27; review findings and upstream unknowns resolved 2026-10-30; upstream re-checked 2026-11-03.
Vocabulary: `docs/ual/CONTEXT.md`. Decisions with rationale: `docs/ual/adr/0001`–`0008`.

## 1. Shape of the integration

Aiken becomes a **UAL producer** and nothing more (ADR-0001). It reads annotations from `.ak` source, compiles as usual, and emits the documents the UAL 1.0 consumer expects. PlutusCoreBlaster is the **consumer**; Aiken drives it in one command and otherwise has no dependency on Lean or Z3.

```
.ak source ──aiken verify generate──▶ assurance/plutus.json               (compiled-interface dialect)
   │                                  assurance/assurance.json
   │                                  assurance/checking-<property>.json  (one per property)
   │
   └─aiken verify check────────────▶ #verify_blueprint in the consumer workspace
                                        ──▶ per-property verdicts, exit code
                                        ──▶ assurance/verified-assurance.json  (only when every property verified)
```

The root `plutus.json` written by `aiken build` is never touched (ADR-0006).

UAL and PlutusCoreBlaster move weekly. As of 2026-10-30 the consumer is on PlutusCoreBlaster `main` (PR #53, commit `1c23de8`), and UAL is at specification 1.0 (`1f7ab84`). 1.0 renames 0.6-draft with no content change and adds temporal operators, which are out of scope here. The plan pins the consumer's own workspace lock, vendors its schemas, and runs a drift job (§8).

## 2. Surface syntax

There are three constructs. Each maps 1:1 onto a UAL annotation at the document level.

### 2.1 Annotation blocks (`//@ … @//`) — ADR-0002

The lexer gets a new trivia kind. It sees `//@`, swallows everything up to the first `@//` that is at line start or preceded by whitespace, and records the span in `ModuleExtra` as `AssuranceComment`, next to the existing comment kinds. There is no token and no AST node. `aiken fmt` re-interleaves the span verbatim and must not reindent its interior. A `//@` inside a region, or an unterminated region, is a lexer error.

The first word of a block is its kind. Keywords are UAL's own, uppercase, with no aliases.

```aiken
//@ PREDICATE
def ownerAuthorized (owner : PubKeyHash) (ctx : ScriptContext) : Prop :=
  txSignedBy owner ctx.scriptContextTxInfo
@//

//@ PROPERTY owner_signature_required for vesting.spend
  "A transaction the owner has not signed fails script evaluation."
  ∀ (p : VestingParams) (ctx : ScriptContext),
    ¬ txSignedBy p.owner ctx.scriptContextTxInfo →
      isUnsuccessful (vesting__vesting p (toLedgerData ctx))
@//
```

The property header is `PROPERTY <id> for <scope>+ "<description>" <lean prop>`:
- `<id>` matches `[A-Za-z0-9_-]+` and is unique across the project.
- Scope entries are Aiken target names (`vesting`, `outbids`); for validators a scope entry may carry a `.<handler>` suffix (§3.3). Multiple scopes are separated by whitespace.
- The description is mandatory and non-empty, per spec.
- The proposition calls targets by their emitted ids (§3.2, ADR-0007).

The consumer accepts a closed subset of Lean expression syntax in propositions and predicates: plain `def`/`abbrev`; no tactics, attributes or `by`. The producer passes text through unchanged and the consumer enforces the restriction. Until PlutusCoreBlaster #74 merges, write ordering as `≤`/`≥`; ASCII `<=`/`>=` are rejected.

Blocks may appear anywhere at module top level. A property's scoped targets must be defined in the same module. This is a v1 restriction; relaxing it to "any module that `use`s the target" would be additive.

### 2.2 `@onchain` decorator — ADR-0003

This extends the existing decorator machinery (`decorators()`, today only on data types) to `fn` and `validator`. The decorator carries execution settings only; the producer derives the ONCHAIN signature.

```aiken
@onchain(steps: 100, semantics: e)
pub fn outbids(old: Int, proposed: Int) -> Bool { … }

@onchain(steps: 2500, semantics: e)
validator vesting(params: VestingParams) { spend(…) { … } else(_) { fail } }
```

New type-checker rules, in `tipo/infer.rs` alongside the validator checks:
- `@onchain` is allowed only on `fn` and `validator`; anywhere else it is an error.
- `steps` is a mandatory positive integer. `semantics` is mandatory and must be `c` or `e`, the only variants the consumer accepts for Plutus V3; `e` is post-Conway. Duplicate or unknown keys are errors.
- Function targets may not have opaque types in parameters or result, free type variables, or function-typed parameters. These are the same rules as `aiken export`, surfaced at check time with the same error kinds.
- A function in scope of a `PROPERTY` without `@onchain` is an error with a fix-it.

### 2.3 UPLC_DATA — implicit

Every type reachable from a target's signature is emitted into `definitions`, using the closure the blueprint already computes. There is no keyword.

## 3. Lowering to UAL documents

### 3.1 Derived ONCHAIN signature

These facts were verified in Aiken source at HEAD `2a04753`. `generate_raw` wraps each argument with `cast_validator_args`, so every function-target argument arrives as Data, and the result is returned native. Validators take parameters as Data and a raw-Data context.

| Aiken position | Aiken type | Emitted encoding | Schema |
|---|---|---|---|
| fn argument | `Data` | raw Data (identity) | `#/definitions/Data` |
| fn argument | anything else | `asData` | blueprint schema of the type |
| fn result | `Int` / `ByteArray` / `String` / `Bool` / `Void` | `asNative` | `#integer` / `#bytes` / `#string` / `#boolean` / `#unit` |
| fn result | `List<_>`, `Pair<_,_>`, tuples | `asNative` list/pair **of Data** elements | `#list` / `#pair` |
| fn result | ADT, `Option`, `Data` | Data | blueprint schema |
| validator parameter | any | `asData` | `/parameters/i` schema |
| validator runtime input | — | raw Data context (V3 convention) | — |

`plutusVersion` is always `v3`, since Aiken is V3-only. The consumer binds native `#list`/`#pair` to raw Data elements, which is exactly Aiken's representation (confirmed in the PlutusCoreBlaster README).

### 3.2 Target ids and digests — ADR-0007

- **Ids.** The consumer requires `^[A-Za-z0-9_-]+$` for validator, function and property ids. The emitted id of Aiken target `module/path.name` is the module path with `/`, `-` and `.` (dotted file names) replaced by `_`, then `__`, then the name. For example, `auction/lib.outbids` becomes `auction_lib__outbids`, and validator `vesting` in module `vesting` becomes `vesting__vesting`. If two targets map to the same id, the producer reports an error naming both. The consumer uses this id as the Lean wrapper name (`<id>` for the CEK state, `<id>_returns` for an exact result), so it is also the name a proposition calls. One function in the producer owns this mapping. Before writing a bundle, the producer also checks every Lean name the consumer declares in each property's environment (validator title, id, `_hash`, `_paramCount` and wrapper `_script` bindings; function `_script` and `_returns`; single-segment helpers) and rejects duplicates, naming both sources. ADR-0007 lists the inventory.
- **Function digests.** SHA-256 over the hex-decoded `compiledCode`, which is single CBOR wrapping flat. This is what the consumer computes, and it is Aiken's existing `compiledCode` encoding. Functions are registered only in the assurance document's `functions` registry and never copied into the blueprint. A function id must not equal a validator id.
- **Script hash.** The blueprint script hash (blake2b-224, as today).

### 3.3 Validator targets and purposes — ADR-0004

One target per `validator`: the whole program, which already contains the purpose dispatch. Its interface lists **all six** V3 invocations (spend, mint, withdraw, publish, vote, propose). The compiled program can be invoked under every purpose; unhandled ones fail through Aiken's fallback, which is itself behaviour a property may state.

Each validator checking target must name a `purpose`. In V3 every invocation has the same single raw-Data context argument, so the selected purpose picks the calling convention but does **not** constrain the context. The semantics of a handler-scoped property therefore come from a premise. A scope `v.<handler>` lowers to:
- a premise on `ctx.scriptContextScriptInfo`, written into the **complete proposition** that lands in `assurance.json`;
- `purpose` = that handler's purpose in the checking target. For `else` and for unsuffixed scopes, `purpose` is the first in canonical order (spend), which has no semantic effect in V3.

| Aiken handler | Premise added |
|---|---|
| `spend` | `∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d` |
| `mint` | `∃ (cs : _), … = MintingScript cs` |
| `withdraw` | `∃ (cred : _), … = RewardingScript cred` |
| `publish` | `∃ (i : _) (cert : _), … = CertifyingScript i cert` |
| `vote` | `∃ (voter : _), … = VotingScript voter` |
| `propose` | `∃ (i : _) (pp : _), … = ProposingScript i pp` |
| `else` | conjunction of the negations of every handler `v` defines |

**Composing scopes.** A property's scopes all constrain the one `ctx`, and their premises are conjoined. Each selector admits a set of purposes over the six V3 `ScriptInfo` constructors:
- an unsuffixed validator scope, or a function scope, admits all six (no premise);
- `v.h` admits `{h}`;
- `v.else` admits all six minus `v`'s explicit handlers.

The conjunction holds exactly when some purpose is admitted by every selector. The six constructors are disjoint and exhaustive, so intersecting the sets is exact for the generated premises, not an approximation. The producer rejects a property whose intersection is empty (`v.spend v.mint`; `v.spend v.else` when `v` defines spend; `v.spend w.mint`; `v.else` when `v` handles all six purposes). It reports a source-located diagnostic before any code generation or file is written, rather than publishing a vacuous claim. Selectors across validators follow each validator's own handlers: `v.spend w.else` is accepted when `w` defines no spend handler. The check covers only the premises Aiken generates. A user-written antecedent can still be unsatisfiable, and the producer never tries to decide that. Disjunctions of purposes are written explicitly in the proposition under an unsuffixed scope; the producer never turns conjunction into disjunction.

Constructor names come from CardanoLedgerApi V3 `Contexts.lean`. UAL-13 found that the pinned consumer rejects unbracketed existential binders; bracketed binders with inferred types are accepted. Both this form and an equivalent match-based premise discharge fixture 2's spend/else properties. Retain the existential form: it verified successfully and the match form showed no discharge advantage. See [the empirical results](e2e-results.md). The check driver opens `CardanoLedgerApi.V3` and `CardanoLedgerApi.V3.Contexts.ScriptInfo` for annotation names and generated constructors; imports alone do not open these namespaces.

### 3.4 Predicates, fragments, namespaces

- Four names are kept distinct. The **Aiken module** is the project-unique module path (`[a-z0-9_./]`). The **fragment id** maps `/` to `.` and `.` to `-` (`proofs/helpers` → `proofs.helpers`, `a.b` → `a-b`). It is injective over module names and always matches the fragment-id schema `^[A-Za-z0-9_.-]+$`; ids of plain-identifier paths equal their Lean spelling, as before. The **Lean namespace** keeps `/`-separated segments and escapes a segment `«…»` only when it is not a valid Lean identifier (`example` → `«example»`, `a.b` → `«a.b»`). A **helper reference** is the namespace, `.`, and the declared name.
- All PREDICATE blocks of a module concatenate, in source order, into one fragment. The fragment's `imports` are the module's `use` edges to modules that have fragments, followed transitively through modules without. The graph is acyclic by construction.
- The fragment source is the blocks verbatim, then a copy of each block in which every `def`/`abbrev` name is prefixed with the module namespace. The pinned consumer elaborates every fragment in one driver namespace and accepts only `def`/`abbrev` commands, so `namespace … end`, `open` and `@`-aliases are unavailable, and a plain `abbrev` alias fails for implicit parameters. Lean elaborates a dotted declaration with its prefixes open, so sibling references in a copy resolve to the qualified siblings. Declared names keep working unqualified across the closure; qualified names are the documented form for imported helpers. Aiken `use … as` aliases are not Lean names.
- A module containing only annotation blocks and `use` lines is valid. The unused-import warning is suppressed when an annotation references the imported namespace.
- In each property's fragment closure, every emitted name (declared and qualified) must belong to one helper. A copy must also resolve each emitted name to the same helper as its declaration: Lean tries the innermost open prefix first. Both are producer errors, because the spec forbids silent merging or rebinding.
- A missing-fragment error requires a lexical name that spells a project helper's qualified declaration exactly, or projects from it. A bare namespace prefix is not evidence of a module reference, because `ctx.scriptContextTxInfo` is normally a projection of a bound variable. Unqualified and unknown names are left to the consumer.

### 3.5 Properties

Each property gets one `checking-<property-id>.json`. It carries:
- `{"semanticsVariant": …, "budget": {"kind": "cek-steps", "steps": n}, "acceptance": "evaluation"}` from the target's `@onchain`;
- the target digests and, for validators, the selected purpose;
- the function interface copy, for function targets;
- `parameters.mode: universal`, or `applied` with all parameter artifacts and the specialized script identity (ADR-0008);
- the environment manifest hash.

All targets in one property must share execution settings (spec).

Applied-parameter checking (`mode: applied`, UAL-14) uses a per-scope `[CBOR_HEX,...]` suffix after the target/handler name. Values follow declaration order and use `aiken blueprint apply`'s Plutus Data CBOR input form. The property's wrapper takes runtime arguments only; the template remains unchanged. See [ADR-0008](adr/0008-property-local-applied-parameters.md).

Before any file is written, the producer validates:
- unique ids;
- known scopes;
- each scoped target is defined in the property's own module;
- arity of the derived signature;
- the fragment closure provides every referenced helper;
- the definitions graph is finite with no reference-only cycles.

The producer never emits evidence.

## 4. Commands

`aiken build`, `check` and `export` are unchanged and never require Lean.

```
aiken verify generate [--environment <environment.json>] [--no-environment]
aiken verify check    [--environment <environment.json>]
aiken verify env      [--out <environment.json>]
```

- **`generate`** compiles, runs the assurance pass, validates, and writes `assurance/plutus.json`, `assurance/assurance.json` and the checking contexts. `--no-environment` runs the whole pass and its diagnostics but writes **nothing**: a bundle without an environment binding is unrunnable and must not exist on disk.
- **`check`** works the way the consumer's own runners do, since `#verify_blueprint` has no machine-readable output mode:
  1. Run `generate`.
  2. Write a Lean driver: `import PlutusCore.UPLC.BlueprintEncoding.Assurance`, `import CardanoLedgerApi.V3`, `open CardanoLedgerApi.V3 CardanoLedgerApi.V3.Contexts.ScriptInfo`, then `#verify_blueprint <ns> "<assurance.json>"`.
  3. Run it with `lake env lean` in the consumer workspace (§4.1).
  4. Read verdicts. Prefer the JSON verdict report (`(report := "<path>")`, format `blaster-verification-report-v1`) once PlutusCoreBlaster #69 merges. Until then, parse the per-property log lines: `Property '<id>': verified (Blaster SMT; no reconstructed Lean proof)`, `falsified`, `inconclusive`, `not checked (…)`, and the summary line. Any property without a recognised verdict, a non-zero Lean exit, or unparseable output is a failure.

  The exit status is non-zero unless every property is `verified`. Only then is `assurance/verified-assurance.json` written, carrying `smt-check` evidence bound to each `checkingContextHash`. Output goes through the existing `telemetry::EventListener`, so it is styled on a TTY and JSON otherwise, with the schema via `--show-json-schema`, exactly like `aiken check`. Every run states the trust base:
  - Blaster SMT, with no reconstructed Lean proof;
  - the Z3 binary;
  - the PlutusCoreBlaster, CardanoLedgerApi and Blaster revisions;
  - that results are relative to the step budget.

  For exact-result (`_returns`) claims, `falsified` means a counterexample has **no matching result within the declared step budget**. A too-small budget can therefore falsify the bounded claim even when the program would return the expected result with more steps. It does not establish a completed wrong result or unbounded failure; verified results remain relative to the declared budget.

  It warns when a property scopes a helper used by a validator but no property scopes that validator.
- **`env`** writes and runs a driver with the same imports and `#write_checking_environment "<out>"`.

### 4.1 Consumer workspace

The supported consumer is the coordinated workspace PlutusCoreBlaster pins in `scripts/assurance-workspace.json`, with sibling checkouts `PlutusCoreBlaster`, `Lean-blaster` and `CardanoLedgerApiBlaster`. At `1c23de8` the pins are:

| Component | Pin |
|---|---|
| Lean | 4.24.0 |
| Blaster | `bc03bb4` |
| CardanoLedgerApiBlaster | `997b9ca` (branch `feat/ual-property-helpers`; `toLedgerData` is not yet on its `main`) |
| UAL | `1f7ab84` |
| Z3 | a build with the `recfun-finder` simplifier |

These pins move when PlutusCoreBlaster #70 merges (Blaster → `1cb5a63`, CardanoLedgerApiBlaster → `5492bb0`); Aiken follows the lock, not these literal values. Ticket UAL-18 adds a script that builds this workspace inside a job's sandbox. The worker image already provides Lean 4.24.0 and the assurance Z3 at `ca39e58` with `recfun-finder`; workers cannot download toolchains.

`aiken.toml` gets `[assurance] workspace = "<path>"`, falling back to an environment variable. Aiken vendors that lock file and reports mismatches; it does not manage the workspace.

## 5. Code layout

- **`crates/aiken-ual`** (new, no dependency on `aiken-lang`): the UAL document model (assurance-v2, checking context, compiled-interface blueprint dialect, environment manifest, verified-assurance evidence), a canonical serializer, self-validation, and SHA-256 helpers. It vendors the schemas that ship in PlutusCoreBlaster `PlutusCore/UPLC/BlueprintEncoding/*.schema.json` at the pinned commit (assurance-v2, checking-context, interface, value, blueprint; upstream source `RSoulatIOHK/CIPs@b12d4b0`), plus `assurance-workspace.json`. It is reusable by other producers.
- **`crates/aiken-lang`**: the lexer region and `ModuleExtra::assurance_comments`; `@onchain` decorator parsing on `fn`/`validator`; `DecoratorKind::Onchain { steps, semantics }`; the type-check rules; formatter passthrough.
- **`crates/aiken-project/src/assurance/`**: the block parser (header grammar, Lean body as opaque text); scope resolution against the typed module; id mapping (ADR-0007); signature derivation (reuses `blueprint::schema` and `export`); purpose lowering; the fragment graph; bundle assembly; the subcommands' project-side logic, including driver generation and verdict parsing.
- **`crates/aiken/src/cmd/verify/{generate,check,env}.rs`**.
- **`crates/aiken-project/src/blueprint/`**: compiled-interface dialect emission (one entry per validator with six invocations, stable ids, no `arguments`/`budget` keys), written only to `assurance/plutus.json` (ADR-0006).

## 6. Conformance fixtures

These mirror the UAL acceptance criteria. Phase 1 compares emitted JSON against reviewed expected documents; phase 2 runs the fixtures through the consumer.

1. A function target with a native result (`outbids`), with one verified property.
2. A validator target with `spend` and `else`, and purpose-scoped properties, both universal over parameters and an applied variant binding concrete values.
3. A property that must be **falsified**, whose counterexample surfaces in `verify check`.
4. A recursive Data type through a function target, so the `definitions` graph round-trips.
5. Negative cases:
   - unknown scope;
   - property in the wrong module;
   - missing fragment;
   - opaque parameter;
   - missing `steps`;
   - a semantics variant other than `c`/`e`;
   - id collision after mapping;
   - altered bytes or hash mismatch;
   - one-step exhaustion, reported as **falsified** with no evidence.

Bounded-semantics decision (2026-11-09): at PlutusCoreBlaster `1c23de8`, `_returns` is `False` for every CEK state other than a matching `Halt` within the step budget. “Returns true within 1 step” is genuinely false for the native fixture, so **falsified**, not inconclusive, is the correct verdict. A verified result is unaffected. The earlier inconclusive expectation was our assumption, not a consumer contract. The end-to-end script asserts the exact falsified verdict and absence of evidence, and must fail if a future pin changes it. A separate exhaustion verdict is an upstream request tracked by **UAL-21**, not a prerequisite for UAL-13; see [the results and reproducer](e2e-results.md).

## 7. Phases (one branch, one draft PR)

All of this ships as a single PR on aiken-lang/aiken, run as one plan in the agent system. The plan's integration branch is `agent/ual-verify`, created from `main`, and the plan's single draft PR targets `main`. Tickets run one at a time as jobs: each job gets the branch as earlier tickets left it, and the agent system commits its result onto the branch. The first push opens the draft PR; later pushes add commits to it. Workers never create branches or open PRs. Bringing in `main` is a maintainer step (GitHub's "Update branch"), not a ticket, because the system pushes a job's file changes, not git history. The PR moves from draft to ready only at the end. Upstream changes (PlutusCoreBlaster #69 and the like) happen outside the plan, in their own repos. Site documentation (aiken-lang/site) is a follow-up after merge.


**Phase 1 — the producer, testable without Lean.** In order:
1. `aiken-ual` crate and vendored schemas
2. lexer region and formatter passthrough
3. `@onchain` decorator and type-check rules
4. assurance pass (parse, resolve, derive, lower, validate)
5. blueprint dialect emission
6. `aiken verify generate` with dry run
7. fixtures 1, 2, 4 and 5 as JSON snapshots
8. docs

**Phase 2 — consumer wiring.** The consumer has been on PlutusCoreBlaster `main` since 2026-10-30, so phase 2 is no longer gated on it. In order:
1. a script that builds the consumer workspace inside a job sandbox
2. `aiken verify env`
3. `aiken verify check`, with telemetry events and the exit rule
4. fixture 3, and end-to-end runs of 1, 2 and 4
5. drift job (§8)
6. move the PR from draft to ready

## 8. Tracking upstream drift

A local hermes job (not CI) runs on a schedule. It:
- pulls `UniversalAnnotationLanguage`, `PlutusCoreBlaster` and `CardanoLedgerApiBlaster`;
- diffs the vendored schemas, `assurance-workspace.json`, the consumer's verdict log formats and the V3 `ScriptInfo` constructors against the pins;
- regenerates and re-checks the fixtures;
- reports state changes on the open upstream PRs listed in §9;
- hands anything that is not a pure re-pin to an agent worker, with the diff.

The pinned UAL version is written into every emitted document, so a stale producer is visible to the consumer.

## 9. Upstream facts (resolved 2026-10-30)

Every question previously listed here was answered from upstream source. The answers are folded into the sections above.

| Question | Answer | Source |
|---|---|---|
| May the producer derive ONCHAIN signatures? | Yes. The consumer only sees documents, and its checks (interface equality, encodings, digests) apply regardless of who wrote the signature. | `Assurance.lean`, `Basic.lean` |
| `#list` with Data elements? | Yes. Native `#list`/`#pair` bind raw Data elements. | PlutusCoreBlaster `Tests/BlueprintVerify/README.md` |
| Purpose: premise or checking-context selection? | Both. Selection is required and picks the invocation, but in V3 it does not constrain the context, so the premise carries the meaning. | `Basic.lean` (`selectSlot`, invocations), `Assurance.lean` |
| Report contract | No machine-readable mode. `#verify_blueprint` logs a per-property verdict; the Plinth runners regex these lines and write `verified-assurance.json` only on full success. Aiken does the same. | `Assurance.lean` (verdict messages), `run_generated.py` |
| Schema location; consumer status | Schemas ship in PlutusCoreBlaster `PlutusCore/UPLC/BlueprintEncoding/`. The consumer was merged to `main` 2026-10-30 (`1c23de8`). | PlutusCoreBlaster git history |
| Target id characters | `^[A-Za-z0-9_-]+$`. Wrappers are named from the id via `sanitizeName`. | `assurance-v2.schema.json`, `interface.schema.json`, `Basic.lean` |
| Function digest bytes | SHA-256 over the hex-decoded `compiledCode` (single CBOR wrapping flat). | `Assurance.lean` |
| Semantics variant for V3 | `C` or `E`; `E` is post-Conway. | `Basic.lean` `validateSemantics` |
| Consumer toolchain | The workspace lock in §4.1. | `scripts/assurance-workspace.json` |

### Open upstream PRs that affect this plan (as of 2026-11-03)

None of these block the plan; each has a fallback.

| PR | What it changes | Effect on us | Until it merges |
|---|---|---|---|
| PlutusCoreBlaster #69 (ours) | Opt-in JSON verdict report, `(report := "…")` | `verify check` reads verdicts from JSON | Parse log lines |
| PlutusCoreBlaster #70 | Typed counterexamples via a positional `cex_report "…"`; bumps Blaster and ledger pins | Same syntax line as #69, so the two need reconciling; first re-pin of §4.1 | Current pins |
| PlutusCoreBlaster #74 | Accepts ASCII `<=`/`>=` in propositions | Users may write ASCII ordering | Use `≤`/`≥` |
| CardanoLedgerApiBlaster #14 (draft) | UAL helpers, including `toLedgerData` (exported from V3), onto `main` | Pin a `main` revision instead of a branch | Pin `997b9ca` |
| UAL #3 | Property templates, starting with purpose isolation | Stretch ticket UAL-19; confirms ADR-0004 | Write the property by hand |

Fixing PR #69's review findings, and coordinating #69/#70 and CardanoLedgerApiBlaster #14, are maintainer tasks outside the plan's board.

## 10. Explicitly out of scope

- Lifting property-based tests into properties (ADR-0005).
- Source positions in emitted documents (needs source maps Aiken does not have).
- `@onchain` on `test`.
- Default step budgets.
- Scott encodings.
- Ledger ex-unit budgets (the consumer rejects them).
- Properties scoped to targets in other modules (v1).
- UAL 1.0 temporal operators for state-machine properties.
- Helper-to-inlined-code equivalence. The consumer does not claim it either.
- Ledger execution-cost (ex-unit) checking, which PlutusCoreBlaster #75 begins to model, and UPLC-to-UPLC equivalence checks (`uplc_equiv`, CardanoLedgerApiBlaster #20). Both are candidates for later work.
