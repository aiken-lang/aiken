#!/usr/bin/env python3
"""Build the pinned public UAL consumers using installed tools only."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

SCHEMAS = Path(__file__).resolve().parents[2] / "crates/aiken-ual/schemas"


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def run(command, cwd=None, *, env=None, input=None):
    result = subprocess.run(command, cwd=cwd, env=env, input=input, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    require(result.returncode == 0,
            f"Command failed ({result.returncode}): {' '.join(map(str, command))}\n{result.stdout}")
    return result.stdout.strip()


def sha256(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def preflight(lock):
    """Bypass elan proxies entirely: even `elan run` may check for updates."""
    for name in ("git", "z3"):
        require(shutil.which(name), f"Missing {name}; use the provisioned worker image. No installation attempted.")
    toolchain = "leanprover/lean4:v" + lock["lean"]
    elan_home = Path(os.environ.get("ELAN_HOME", Path.home() / ".elan")).expanduser()
    lean = elan_home / "toolchains" / ("leanprover--lean4---v" + lock["lean"]) / "bin/lean"
    require(lean.is_file() and (lean.parent / "lake").is_file(),
            f"Missing installed Lean {lock['lean']} / Lake in {lean.parent}; no download attempted. "
            "Use the provisioned worker image or set ELAN_HOME to its installed toolchains.")
    lean = lean.resolve()
    version = run([str(lean), "--version"])
    require(re.search(r"\bversion " + re.escape(lock["lean"]) + r"(?:,|\s)", version),
            f"Lean version mismatch: expected {lock['lean']}, got {version}. No installation attempted.")
    env = os.environ.copy()
    env["PATH"] = str(lean.parent) + os.pathsep + env["PATH"]
    env["ELAN_TOOLCHAIN"] = toolchain
    env["GIT_OPTIONAL_LOCKS"] = "0"
    env["GIT_TERMINAL_PROMPT"] = "0"
    solver = Path(shutil.which("z3")).resolve()
    try:
        probe = subprocess.run([str(solver), "-in"],
                               input=f"(set-simplifier {lock['z3RequiredSimplifier']})\n(check-sat)\n",
                               text=True, capture_output=True, timeout=15)
    except subprocess.TimeoutExpired as error:
        raise RuntimeError("Z3 recfun-finder probe timed out; use the provisioned assurance Z3. No installation attempted.") from error
    require(probe.returncode == 0 and probe.stdout.strip() == "sat" and not probe.stderr.strip(),
            "Z3 lacks the required recfun-finder capability; put the assurance z3 (not z3-4.15.2) on PATH. "
            f"No installation attempted.\n{probe.stdout}{probe.stderr}")
    return env, {"lean": version, "leanPath": str(lean),
                 "z3": run([str(solver), "--version"]), "z3Sha256": sha256(solver)}


def checkout(path, url, revision, env, verify_only):
    if not path.exists():
        require(not verify_only, f"Completed workspace is missing {path}; use a fresh workspace")
        print(f"Cloning {url} at {revision}", flush=True)
        run(["git", "init", "--quiet", str(path)], env=env)
        run(["git", "remote", "add", "origin", url], path, env=env)
        run(["git", "fetch", "--depth=1", "origin", revision], path, env=env)
        run(["git", "checkout", "--detach", "FETCH_HEAD"], path, env=env)
    require((path / ".git").is_dir(), f"Not a standalone checkout: {path}")
    actual = run(["git", "rev-parse", "HEAD"], path, env=env)
    require(actual == revision, f"Revision mismatch at {path}: expected {revision}, got {actual}; refusing to reset it")
    status = run(["git", "status", "--porcelain", "--untracked-files=normal"], path, env=env)
    require(not status, f"Checkout is dirty: {path}\n{status}\nUse a fresh workspace; existing files are never reset.")


def overrides(repo, packages, verify_only):
    path = repo / ".lake/package-overrides.json"
    desired = {"version": "1.1.0", "packages": [
        {"type": "path", "name": name, "scope": "", "inherited": False,
         "dir": str(directory), "configFile": "lakefile.lean", "manifestFile": "lake-manifest.json"}
        for name, directory in packages
    ]}
    if path.exists():
        require(json.loads(path.read_text()) == desired, f"Lake overrides differ: {path}; refusing to overwrite")
    else:
        require(not verify_only, f"Completed workspace is missing {path}")
        path.parent.mkdir(exist_ok=True)
        path.write_text(json.dumps(desired, indent=2) + "\n")


def artifacts(workspace, repos):
    """Cheap read-only completeness/staleness check, not an artifact attestation."""
    result = {}
    for repo in repos:
        root = repo / ".lake/build"
        require(root.is_dir(), f"Missing build artifacts: {root}")
        for path in sorted(root.rglob("*")):
            if path.is_file():
                stat = path.stat()
                result[str(path.relative_to(workspace))] = [stat.st_size, stat.st_mtime_ns]
    return result


def check_verdicts(output, expected):
    found = re.findall(r"Property '([^']+)': ([^\n\r]*)", output)
    # This progress message precedes each final verdict at the pinned consumer.
    found = [(name, verdict) for name, verdict in found if verdict != "checking fresh proposition"]
    require(len(found) == len(expected) and {name for name, _ in found} == set(expected)
            and all(verdict == "verified (Blaster SMT; no reconstructed Lean proof)" for _, verdict in found),
            f"Game smoke did not verify every expected property: {found}")
    return [f"Property '{name}': {verdict}" for name, verdict in found]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workspace", type=Path,
                        default=Path(os.environ.get("UAL_WORKSPACE", Path.home() / "ual-workspace")),
                        help="sibling checkout root (default: UAL_WORKSPACE or ~/ual-workspace)")
    args = parser.parse_args()
    start = time.monotonic()
    lock_bytes = (SCHEMAS / "assurance-workspace.json").read_bytes()
    lock = json.loads(lock_bytes)
    pin = dict(line.split(": ", 1) for line in (SCHEMAS / "PIN").read_text().splitlines() if ": " in line)
    require(lock["format"] == "assurance-workspace-v1", "Unsupported workspace lock format")
    env, tools = preflight(lock)
    workspace = args.workspace.expanduser().resolve()
    core = workspace / "PlutusCoreBlaster"
    blaster = (core / lock["layout"]["blaster"]).resolve()
    ledger = (core / lock["layout"]["ledger"]).resolve()
    require(blaster.parent == ledger.parent == workspace and len({core, blaster, ledger}) == 3,
            "Lock must describe three distinct sibling checkouts")
    repos = [core, blaster, ledger]
    receipt = workspace / ".ual-workspace-ready.json"
    verify_only = receipt.exists()
    identity = {"format": 1, "coreCommit": pin["commit"],
                "lockSha256": hashlib.sha256(lock_bytes).hexdigest(), "tools": tools}
    saved = json.loads(receipt.read_text()) if verify_only else None
    if saved:
        require(saved["identity"] == identity, "Workspace pins or tools changed; use a fresh workspace")
    workspace.mkdir(parents=True, exist_ok=True)
    for path, url, revision in [
        (core, pin["repository"], pin["commit"]),
        (blaster, "https://github.com/input-output-hk/Lean-blaster", lock["blasterCommit"]),
        (ledger, "https://github.com/" + lock["repositories"]["ledger"]["repository"],
         lock["repositories"]["ledger"]["commit"]),
    ]:
        checkout(path, url, revision, env, verify_only)
        require((path / "lean-toolchain").read_text().strip() in
                (lock["lean"], "v" + lock["lean"], env["ELAN_TOOLCHAIN"]),
                f"Lean toolchain declaration differs from lock: {path}")
    require((core / "scripts/assurance-workspace.json").read_bytes() == lock_bytes,
            "Vendored lock differs from pinned PlutusCoreBlaster lock")
    overrides(core, [("Blaster", blaster)], verify_only)
    overrides(ledger, [("Blaster", blaster), ("PlutusCore", core)], verify_only)
    if verify_only:
        require(saved["artifacts"] == artifacts(workspace, repos),
                "Build artifacts changed or are missing; use a fresh workspace (no automatic rebuild)")
        print("PASS existing workspace: pins, clean sources, tools, overrides and build artifacts unchanged")
        print("Cached Game verdicts from the completed build (not re-run):")
        print("\n".join(saved["verdicts"]))
    else:
        for repo, targets in [(core, ["Blaster:shared", "PlutusCore.UPLC.BlueprintEncoding.Assurance"]),
                              (ledger, ["CardanoLedgerApi"])]:
            print(f"Building {repo.name}: {' '.join(targets)}", flush=True)
            subprocess.run(["lake", "--no-cache", "build", *targets], cwd=repo, env=env, check=True)
        output = run(["lake", "env", "lean", "Tests/BlueprintVerify/Game/Check.lean"], core, env=env)
        print(output, flush=True)
        expected = [p["id"] for p in json.loads((core / "Tests/BlueprintVerify/Game/assurance.json").read_text())["properties"]]
        verdicts = check_verdicts(output, expected)
        receipt.write_text(json.dumps({"identity": identity, "verdicts": verdicts,
                                       "artifacts": artifacts(workspace, repos)}, indent=2) + "\n")
        print("PASS built consumers and verified every Game property")
    print(f"Consumer workspace (UAL-11): {ledger}")
    print(f"Completed in {time.monotonic() - start:.1f}s")


if __name__ == "__main__":
    try:
        main()
    except (RuntimeError, OSError, ValueError, KeyError, subprocess.CalledProcessError) as error:
        print(f"workspace-setup: error: {error}", file=sys.stderr)
        sys.exit(1)
