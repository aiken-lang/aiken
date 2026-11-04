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
