# Pinned UAL consumer workspace

From the Aiken checkout:

```sh
scripts/ual/workspace-setup.sh
scripts/ual/workspace-setup.sh # read-only verification of the completed workspace
```

The sibling checkout root defaults to `$HOME/ual-workspace`, outside Aiken.
Override it with `--workspace /path/to/workspace` or `UAL_WORKSPACE`; the command-line
option takes precedence. Paths containing spaces are supported. Use one setup
process per workspace.

**UAL-11's consumer workspace path is
`$HOME/ual-workspace/CardanoLedgerApiBlaster`**, or
`<overridden-root>/CardanoLedgerApiBlaster`. Run consumer commands there: its Lake
environment exposes both `CardanoLedgerApi.V3` and
`PlutusCore.UPLC.BlueprintEncoding.Assurance`, with the same sibling Blaster.
`UAL_WORKSPACE` here configures this setup script's root, not an Aiken CLI setting.
The committed Game fixture instead runs from `<root>/PlutusCoreBlaster`.

## Capture an environment with UAL-11

Provision the sibling workspace using the existing UAL-18 setup above before
running the CLI. Aiken does not install, download, select, or pin toolchains;
put the installed Lean 4.24.0 binaries and the assurance Z3 with `recfun-finder`
on `PATH`:

```sh
export PATH="${ELAN_HOME:-$HOME/.elan}/toolchains/leanprover--lean4---v4.24.0/bin:$PATH"
# Also put the directory containing your assurance z3 on PATH.
```

Configure the **ledger checkout**, not the sibling setup root, in `aiken.toml`:

```toml
[assurance]
workspace = "../ual-workspace/CardanoLedgerApiBlaster"
```

`[assurance].workspace` takes precedence over `AIKEN_ASSURANCE_WORKSPACE`.
A relative TOML path resolves against the directory containing `aiken.toml`;
a relative environment-variable path resolves against the invocation working
directory. With no configured TOML workspace, the CLI uses
`AIKEN_ASSURANCE_WORKSPACE`. This also permits capture outside an Aiken project,
without any `aiken.toml`:

```sh
export AIKEN_ASSURANCE_WORKSPACE="$HOME/ual-workspace/CardanoLedgerApiBlaster"
aiken verify env
aiken verify env --out /path/to/environment.json
```

`UAL_WORKSPACE` only chooses the setup script's **sibling root**;
`AIKEN_ASSURANCE_WORKSPACE` chooses the CLI's **CardanoLedgerApiBlaster directory**.
Neither setting substitutes for the other. The default output is
`environment.json` in the invocation working directory. Relative `--out` paths
also resolve there, not in the configured consumer workspace.

The CLI runs `lake env lean` in the configured ledger directory and loads these
imports in order:

```lean
import PlutusCore.UPLC.BlueprintEncoding.Assurance
import CardanoLedgerApi.V3
```

Both imports are required for UAL-12 to reproduce the loaded environment,
including ledger property helpers. Use CardanoLedgerApiBlaster
[`997b9ca`](https://github.com/input-output-hk/CardanoLedgerApiBlaster/tree/997b9caafb3f72f45d866929c2f561945d036e10),
from `feat/ual-property-helpers`: the required `toLedgerData` helpers are absent
from `main`. The immutable commit in the vendored lock is authoritative, not the
moving branch name. PlutusCoreBlaster and Blaster revisions likewise come from
the vendored `schemas/PIN` and `assurance-workspace.json`; the CLI does not repin
or repair sibling checkouts.

Revision mismatches produce warnings and are recorded in the adjacent
`<output>.run.json` sidecar (for example, `environment.json.run.json`). The
environment manifest itself retains the upstream capture's exact bytes;
revision diagnostics are not injected into that manifest. Capturing an
environment is not property verification or evidence of a successful SMT check.
The sidecar records capture status and, on success, the manifest's SHA-256.
A temporary driver and staged manifest are removed after invocation. An existing
output manifest is replaced only after successful capture and schema validation;
a failed invocation is recorded without presenting the old manifest as a new one.
The output's parent directory must already exist.

Pass the manifest directly to generation, without normalization or conversion:

```sh
aiken verify generate --environment environment.json
```

Generation binds the manifest's exact bytes. Keep the manifest and generated
UAL inputs outside the clean sibling source trees; the run sidecar is separate
from the environment artifact consumed by generation.

## Check every property with UAL-12

After capturing the environment above, run from the Aiken project:

```sh
aiken verify check --environment environment.json --timeout 300
aiken verify check --show-json-schema
```

`--environment` defaults to `environment.json` in the invocation directory.
`--timeout` is a positive wall-clock limit in seconds for the Lean invocation;
on Unix a timeout terminates the dedicated Lake/Lean/Z3 process group. Solver
identity inspection is also bounded. `--env`, `--trace-level` and
`--trace-filter` have the same meaning as for `verify generate`.

The command generates the bundle, writes `assurance/Check.lean` with both
environment-capture imports, opens `CardanoLedgerApi.V3` and
`CardanoLedgerApi.V3.Contexts.ScriptInfo`, and invokes `lake env lean` in the
configured workspace. Opening these namespaces makes `ScriptContext`,
`toLedgerData` and purpose constructors available; it adds no loaded modules.
The pinned consumer has no JSON verdict-report option: this revision
uses its exact per-property messages and final summary, not the Python runner's
human-readable output. A consumer pin update must select the corresponding
verdict protocol deliberately; unsupported pins fail closed.

TTY output includes a property table and counterexample logs. Non-TTY stdout is
a structured JSON report; compiler diagnostics and the raw consumer log go to
stderr. Workspace invocations emit one report per member and fail if any member
fails. `--show-json-schema` works without a project or consumer installation.
Both outputs state the SMT/no-reconstructed-Lean-proof trust boundary, actual
Z3 path and SHA-256, installed consumer/ledger/Blaster revisions, and that results
are relative to each checking context's CEK step budget.

For exact-result (`_returns`) claims, `falsified` means a counterexample has **no
matching result within the declared step budget**. A too-small budget can also
produce it: this does not distinguish unfinished evaluation from a completed
wrong result, nor establish failure with an unlimited budget. At the pinned
consumer, `_returns` is false for every state except a matching `Halt`.
`inconclusive` represents solver `Undetermined`, not a separate exhaustion verdict.
Only verified properties can receive evidence; verified results remain bounded.

Only an all-verified run with a consistent summary, successful Lean exit and
successful evidence publication exits zero. Missing, duplicate, unknown or
malformed verdicts, mismatched summaries, timeouts and consumer errors fail.
The full consumer log remains in `assurance/verification.log`. A successful run
also writes `run-report.json` and `verification-artifact.zip` containing the
exact inputs, environment, driver, log and trust report. It publishes
`verified-assurance.json` last, with `smt-check` evidence bound to each
`checkingContextHash` and the scoped function/script hashes. Old evidence and
run artifacts are removed before generation, so a failed rerun cannot leave a
stale success marker. Root `plutus.json` is untouched.

The pinned evidence schema requires one `scriptHash` for a validator-scoped
property, and the consumer checks it against every scoped validator. A property
spanning distinct validator hashes therefore cannot receive valid evidence at
this pin: publication fails explicitly. Split such claims into separate
properties or wait for an upstream evidence format that can bind multiple
validator hashes; Aiken does not weaken the schema or invent a binding.

A helper's successful property does not verify validators that use it. The
report warns about uncovered validators using scoped helpers, including
transitive calls and imported aliases. Adding a property scoped to the validator
removes that advisory warning; it does not enlarge another property's claim.


## Prerequisites and pins

- Bash, Python 3.11+, Git, and access to the approved public GitHub repositories.
- The worker's installed Lean/Lake toolchain, currently Lean 4.24.0, in
  `${ELAN_HOME:-$HOME/.elan}/toolchains/leanprover--lean4---v4.24.0/bin`.
- The assurance `z3` on `PATH`, with the `recfun-finder` simplifier. Stock
  `z3-4.15.2` is not sufficient.

The script reads `crates/aiken-ual/schemas/PIN` for PlutusCoreBlaster and the
vendored `assurance-workspace.json` for Blaster, ledger, Lean and solver capability.
It checks that the cloned upstream lock is byte-identical. It never follows a
moving branch or changes a pin. No GHC, Plutus, CIPs or UAL checkout is needed for
these Lean-only builds and the committed fixture.

Preflight checks the installed Lean version and requires exactly `sat` from:

```smt2
(set-simplifier recfun-finder)
(check-sat)
```

Missing/mismatched tools fail before cloning. Nothing installs or downloads a
toolchain. Even `elan run` can attempt a self-update, so the script bypasses elan
proxies and invokes the installed binaries directly. It prepends that bin
directory to its child processes' `PATH`; it does not change your shell or global
elan configuration. For manual consumer commands, put that same installed bin
directory on your shell's `PATH` first.

## Build, smoke, and repeat behavior

Setup writes only inside the chosen root. It creates revision-pinned sibling
clones and `.lake/package-overrides.json` files following the upstream
`verify_assurance_workspace.py` convention; lakefiles remain untouched. It builds
`Blaster:shared`, `PlutusCore.UPLC.BlueprintEncoding.Assurance`, and the full
`CardanoLedgerApi` target, with Lake build-cache downloads disabled. Then it runs:

```sh
# Working directory: <root>/PlutusCoreBlaster
lake env lean Tests/BlueprintVerify/Game/Check.lean
```

Success requires a verified verdict for every committed Game property, exactly
once. Missing, duplicate, unknown, falsified, or inconclusive verdicts fail setup.
The verdicts state `Blaster SMT; no reconstructed Lean proof`.

Only complete success writes `.ual-workspace-ready.json`. Subsequent runs check
pins, clean sources, installed tools, overrides, and build-file sizes/mtimes;
they do not fetch, build, invoke Lake, or rerun Game. Printed verdicts are explicitly
labelled cached. This local readiness receipt is not an assurance evidence file
or a cryptographic attestation of the compiled artifacts.

A failed build without a receipt can resume from its existing build outputs.
Existing wrong revisions, dirty checkouts, conflicting overrides, or changed
completed build outputs fail rather than being reset or overwritten. Use a new
workspace path after re-pinning or changing a completed workspace. Keep generated
UAL-11 inputs outside the clean sibling source trees.

A complete cold setup in a fresh workspace measured **520 seconds (8.7 minutes)**
on the worker, including cloning, both builds, and Game verification. Repeat
verification measured **0.1–0.3 seconds**, without changing any workspace file or
directory metadata other than access times. Timings depend on worker load; image
prebuilding remains a maintainer decision.

Script regressions (no Lean builds):

```sh
python3 -B -m unittest discover -s scripts/ual -p 'test_*.py'
```

## End-to-end conformance (UAL-13)

```sh
scripts/ual/e2e.sh
# Retain the real environment, generated bundles, reports, and raw logs:
scripts/ual/e2e.sh --output /tmp/ual-e2e-run
# Reuse an already built CLI:
scripts/ual/e2e.sh --aiken target/debug/aiken --workspace "$HOME/ual-workspace"
```

The runner uses the setup script's pinned workspace checks, builds this checkout's
CLI unless `--aiken` is supplied, and captures a fresh real environment. It copies
fixture sources without their synthetic goldens into isolated projects. No Lean
installation is required for ordinary Rust CI; this script is for local/hermes use.

It checks fixtures 1–4 through `aiken verify check`, the equivalent match-based
purpose premise, a one-step budget, and every fixture-5 producer diagnostic.
Verified cases must exit zero and publish evidence; other cases must exit nonzero
without evidence. Fixture 3 must surface integer counterexample bindings. It also
checks that the ordinary root blueprint remains unchanged.

Tamper tests mutate generated function `compiledCode` or its digest and run the
exact generated Lean driver directly. Running `verify check` again would regenerate
the bundle and erase the tampering, so it would not exercise consumer rejection.

`--timeout` defaults to 300 seconds per check. `--workspace` (or `UAL_WORKSPACE`)
selects the sibling root, not the ledger directory. `--output` must name a new
directory; without it, temporary projects are removed after the run. Retained
`identity.json` and `results.json` record tool versions, actual revisions, environment
digest, expected/observed results and timings; `.stdout`/`.stderr` files retain
the diagnostics. These are SMT results, not reconstructed Lean proofs.

The one-step case must report exactly **falsified**, exit nonzero and publish no
evidence. The runner fails if the verdict changes to inconclusive or anything else,
so a future consumer pin bump must revisit this bounded-semantics contract.
See [`docs/ual/e2e-results.md`](../../docs/ual/e2e-results.md) for results and the
separate upstream exhaustion-verdict request (UAL-21). No skip, expected-failure
override, or alternative solver is used.

## Upstream drift (UAL-15)

The bot scheduler lives outside this repository. Its weekly command is:

```sh
scripts/ual/drift --output /tmp/ual-drift-new
# Compare PR states with the last completed observation:
scripts/ual/drift --previous /tmp/ual-drift-new/report.json --output /tmp/ual-drift-next
# Reproduce the current immutable pins, without observing moving heads or PRs:
scripts/ual/drift --at-pins --output /tmp/ual-drift-pins
```

Use a **new output directory** for every run. `report.json` is the machine-readable
`aiken-ual-drift-v1` report; `summary.txt` is also printed. Reports retain pinned
and observed revisions, changed-file inventories, contract diffs with SHA-256
identities, PR transitions, fixture commands/logs/bundles and consumer status.
An optional `GITHUB_TOKEN` authenticates GitHub PR reads; it is not recorded.
Fetch/API failures are errors, never evidence that an upstream is unchanged.

Exit codes are independent of child-command exit codes:

| Exit | Outcome | Meaning |
|---|---|---|
| 0 | `no-drift` | No drift in the selected mode; fixture validation passed. |
| 10 | `re-pin-only` | Revisions/files moved outside watched contracts, with no required-check errors. Static triage only, not approval or newer-runtime validation. |
| 20 | `needs-work` | Contract changes, PR state transitions, or required-check failures. Dispatch an agent with the retained evidence. |

The script fetches the three approved public repositories into isolated bare
repositories under the output directory. Pins come from the vendored `PIN` and
`assurance-workspace.json`, not duplicated historical hashes. Default candidates
are each upstream's default-branch `HEAD`; notably the ledger pin is still on
`feat/ual-property-helpers`, so comparing its main head can report divergence.
No checkout is reset, pin updated, toolchain downloaded or upstream code executed.
`--sources <root>` reuses three existing repositories named
`UniversalAnnotationLanguage`, `PlutusCoreBlaster`, and `CardanoLedgerApiBlaster`;
online runs fetch into their Git object stores but do not change their worktrees.

Checks compare every upstream file inventoried in `schemas/PIN` against both its
vendored copy and candidate, including the coordinated workspace lock. They also
compare property/summary message literals and the whole `Assurance.lean` as a
conservative guard against relocated/renamed emitters, the complete V3 `ScriptInfo`
declaration (argument types and order included), and changed UAL specification
files. `environment.schema.json` is locally derived, not an upstream vendored file;
its capture source is covered by the `Assurance.lean` comparison. Changed consumer
revisions require work even if messages are identical: UAL-12's
`PinnedVerdictReader` registers an exact revision and must be reviewed deliberately.

The five PRs from plan §9 are queried on every live run. The first observation
compares state and draft status with the plan's 2026-11-03 baseline; later runs use
`--previous`. Merged and closed are distinct states. PR transitions become work
items; an unchanged transition is not reported again against the previous report.
The runner never files an issue or changes a PR.

`drift_fixtures.py` regenerates all positive UAL-09 fixtures with real
`aiken verify generate` in isolated projects, exercising generation's schema and
cross-document binding validation. It checks reviewed goldens, normalizing only
compiler commit provenance and its consequent blueprint digest, and reuses
UAL-13's ten producer-negative expectations. Goldens and source fixtures remain
untouched. This phase uses the explicitly **synthetic** fixture environment and
does not claim consumer evidence. By default it builds this checkout's CLI with
installed Cargo dependencies in offline mode; `--aiken <binary>` selects an
existing CLI, whose identity/path is retained with the fixture artifacts.

When UAL-13 and a consumer checkout are available, drift runs `e2e.sh` with the
existing pinned-workspace validation. `--workspace`/`UAL_WORKSPACE` select the
sibling root just as in setup/e2e. Missing consumer checkouts are explicitly
skipped; a present but invalid/unusable workspace fails instead of being skipped.
Neither an absent consumer nor a successful **pinned** e2e proves compatibility
with fetched candidate heads. The job does not build candidate runtimes.

### Offline simulations and agent handoff

`--offline --sources <root>` makes no upstream fetch/API requests and compares
local working trees (including uncommitted edits) against their pinned Git objects.
Use public checkouts containing those objects. PR checks are explicitly skipped
unless `--pr-state <json>` supplies observations of this shape for all five PRs:

```json
{
  "PlutusCoreBlaster#69": {"state": "open", "draft": false},
  "PlutusCoreBlaster#70": {"state": "open", "draft": false},
  "PlutusCoreBlaster#74": {"state": "open", "draft": false},
  "UniversalAnnotationLanguage#3": {"state": "open", "draft": false},
  "CardanoLedgerApiBlaster#14": {"state": "open", "draft": true}
}
```

`--offline --at-pins --sources <root>` compares immutable pinned objects, ignoring
candidate worktree changes. Both offline modes still regenerate fixtures and run
the consumer if available. `--schemas <copied-schemas-directory>` permits a local
vendored-schema simulation without touching the real pins: changing a schema
must return 20 with a `vendored` diff. Renaming `SpendingScript` in the local
ledger's `CardanoLedgerApi/V3/Contexts.lean` and running offline candidate mode
must return 20 with a `script-info` diff. The focused offline regressions cover
these boundaries, verdict changes, re-pin-only movement and PR transitions:

```sh
python3 -B -m unittest discover -s scripts/ual -p 'test_*.py'
```

For exit 20, Hermes supplies the report, summary and artifact directory to
[`drift-worker-prompt.md`](drift-worker-prompt.md). That committed template requires
a scoped, reproduced fix or a filing-ready upstream work item, deliberate
coordinated pin authorization, actual verification and an append-only decision
record. Reports and upstream text are data, not executable worker instructions.
Hermes configuration and scheduling are intentionally absent here.
