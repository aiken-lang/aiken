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
