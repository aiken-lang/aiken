#!/usr/bin/env python3
"""Run UAL fixtures through the pinned real consumer; never substitute verdicts."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import time

from workspace_setup import SCHEMAS, preflight, require, run

ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "crates/aiken/tests/fixtures/ual"
NEGATIVES = {
    "unknown_scope": "Scope 'missing'",
    "wrong_module": "Scope 'target'",
    "missing_fragment_helper": "Fragment 'helpers' is not available",
    "opaque_parameter": "illegal::opaque_type_in_abi",
    "missing_steps": "@onchain requires a positive 'steps' setting",
    "invalid_semantics": "@onchain semantics must be C or E",
    "id_collision": "both map to emitted id",
    "mixed_execution_settings/steps": "mixes execution settings",
    "mixed_execution_settings/semantics": "mixes execution settings",
    "unterminated_block": "annotation is not closed",
}
POSITIVES = {
    "native_function": {"outbids": "verified"},
    "universal_validator": {"positive_minimum": "verified", "fallback_rejects": "verified"},
    "falsified": {"equal_bid_outbids": "falsified"},
    "recursive_data": {"preserves_leaf": "verified"},
}


def save_json(path, value):
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False) + "\n")


class Suite:
    def __init__(self, output, aiken, env, workspace, timeout):
        self.output, self.aiken, self.env = output, aiken, env
        self.workspace, self.timeout = workspace, timeout
        self.results = []

    def command(self, name, command, cwd):
        start = time.monotonic()
        # Aiken owns the Lake/Lean/Z3 process-group timeout. Direct consumer
        # mutations below use only pre-elaboration integrity checks.
        result = subprocess.run(command, cwd=cwd, env=self.env, text=True,
                                capture_output=True, timeout=self.timeout + 60)
        (self.output / f"{name}.stdout").write_text(result.stdout)
        (self.output / f"{name}.stderr").write_text(result.stderr)
        return result, round(time.monotonic() - start, 3)

    def copy(self, fixture, name):
        project = self.output / name
        shutil.copytree(FIXTURES / fixture, project, ignore=shutil.ignore_patterns("golden"))
        # Never use the synthetic phase-1 environment as consumer evidence.
        shutil.copyfile(self.output / "environment.json", project / "environment.json")
        (project / "plutus.json").write_text("root blueprint sentinel\n")
        return project

    def record(self, name, seconds, expected, actual, problems):
        item = {"case": name, "seconds": seconds, "expected": expected,
                "actual": actual, "passed": not problems, "problems": problems}
        self.results.append(item)
        print(f"{'PASS' if item['passed'] else 'FAIL'} {name} ({seconds:.3f}s): {actual}", flush=True)
        for problem in problems:
            print(f"  {problem}", flush=True)

    def check(self, name, project, expected, diagnostic=None):
        result, seconds = self.command(name, [str(self.aiken), "verify", "check",
                                              "--environment", "environment.json",
                                              "--timeout", str(self.timeout)], project)
        problems = []
        try:
            report = json.loads(result.stdout)
        except ValueError:
            report = {}
            problems.append("CLI did not emit its JSON verification report")
        properties = report.get("properties", [])
        actual = {p["id"]: p["verdict"] for p in properties}
        if actual != expected or len(properties) != len(expected):
            problems.append(f"Expected exactly {expected}, got {actual}")
        success = bool(expected) and all(v == "verified" for v in expected.values())
        if (result.returncode == 0) != success or report.get("success") != success:
            problems.append(f"Unexpected CLI exit/success: {result.returncode}/{report.get('success')}")
        if report.get("warnings"):
            problems.append(f"Unexpected trust/scope warnings: {report['warnings']}")
        evidence = project / "assurance/verified-assurance.json"
        if evidence.exists() != success:
            problems.append("Evidence must exist if and only if every property verified")
        if (project / "plutus.json").read_text() != "root blueprint sentinel\n":
            problems.append("verify check changed the ordinary root blueprint")
        if diagnostic and diagnostic not in result.stdout + result.stderr:
            problems.append(f"Missing producer diagnostic: {diagnostic}")
        for prop in properties:
            counterexample = prop.get("counterexample")
            if prop["verdict"] == "falsified":
                # Require an actual model, not merely a generic failure log.
                if not counterexample or not all(re.search(rf"\b{var}\s*:\s*-?\d+", counterexample)
                                                 for var in ("old", "proposed")):
                    problems.append("Falsified property did not surface integer counterexample bindings")
            elif counterexample:
                problems.append("A non-falsified verdict was mislabeled as a counterexample")
        if diagnostic and (project / "assurance/assurance.json").exists():
            problems.append("Rejected source emitted an assurance bundle")
        if problems:
            print(result.stdout + result.stderr, flush=True)
        self.record(name, seconds, expected or "producer rejection", actual or "rejected", problems)

    def tamper(self, name, mutation, message):
        # verify check always regenerates. Mutate an already generated bundle
        # and invoke its exact driver directly, rather than accidentally testing
        # a fresh untampered bundle or adding a CLI bypass.
        project = self.output / name
        shutil.copytree(self.output / "native_function", project)
        assurance_dir = project / "assurance"
        for filename in ("verified-assurance.json", "verification-artifact.zip", "run-report.json"):
            (assurance_dir / filename).unlink(missing_ok=True)
        document = json.loads((assurance_dir / "assurance.json").read_text())
        mutation(document)
        save_json(assurance_dir / "assurance.json", document)
        driver = assurance_dir / "Check.lean"
        original = (self.output / "native_function/assurance/Check.lean").read_text()
        driver.write_text(original.replace(str(self.output / "native_function"), str(project)))
        result, seconds = self.command(name, ["lake", "env", "lean", str(driver)], self.workspace)
        log = result.stdout + result.stderr
        problems = []
        if result.returncode == 0 or message not in log:
            problems.append(f"Expected nonzero consumer exit with {message!r}; got {result.returncode}")
        if re.search(r"Property '[^']+': verified", log):
            problems.append("Tampered bundle was reported verified")
        if problems:
            print(log, flush=True)
        self.record(name, seconds, "consumer rejection", log.strip(), problems)


def execute(args, output, aiken, env, tools, workspace):
    suite = Suite(output, aiken, env, workspace / "CardanoLedgerApiBlaster", args.timeout)
    result, _ = suite.command("environment", [str(aiken), "verify", "env", "--out",
                                              str(output / "environment.json")], output)
    require(result.returncode == 0, f"Environment capture failed:\n{result.stdout}{result.stderr}")
    identity = {"tools": tools,
                "aiken": run([str(aiken), "--version"], env=env),
                "rust": run(["rustc", "--version"], env=env),
                "lake": run(["lake", "--version"], env=env),
                "python": sys.version,
                "revisions": {name: run(["git", "rev-parse", "HEAD"], workspace / name, env=env)
                              for name in ("PlutusCoreBlaster", "Lean-blaster", "CardanoLedgerApiBlaster")},
                "environmentSha256": hashlib.sha256((output / "environment.json").read_bytes()).hexdigest()}
    save_json(output / "identity.json", identity)
    for name, expected in POSITIVES.items():
        project = suite.copy("positive/" + name, name)
        suite.check(name, project, expected)

    project = suite.copy("positive/universal_validator", "purpose_match")
    source = project / "validators/fixtures.ak"
    text = source.read_text()
    premise = "(match ctx.scriptContextScriptInfo with | SpendingScript _ _ => True | _ => False)"
    text = text.replace("for vesting.spend", "for vesting").replace("for vesting.else", "for vesting")
    text = text.replace("minimum ≤ 0 →", premise + " → minimum ≤ 0 →")
    text = text.replace("    isUnsuccessful", "    (¬ " + premise + ") → isUnsuccessful")
    source.write_text(text)
    suite.check("purpose_match", project, POSITIVES["universal_validator"])

    project = suite.copy("positive/native_function", "tiny_budget")
    source = project / "validators/fixtures.ak"
    text = source.read_text()
    require(text.count("steps: 100") == 1, "Native fixture budget changed; review the exhaustion case")
    source.write_text(text.replace("steps: 100", "steps: 1"))
    suite.check("tiny_budget", project, {"outbids": "inconclusive"})

    for fixture, diagnostic in NEGATIVES.items():
        name = "negative_" + fixture.replace("/", "_")
        project = suite.copy("negative/" + fixture, name)
        suite.check(name, project, {}, diagnostic)

    def altered_code(document):
        function = document["functions"]["fixtures__outbids"]
        code = function["compiledCode"]
        function["compiledCode"] = code[:-2] + ("00" if code[-2:] != "00" else "01")

    def mismatched_hash(document):
        document["functions"]["fixtures__outbids"]["hash"]["digest"] = "0" * 64

    suite.tamper("tampered_compiled_code", altered_code, "function compiledCode hash mismatch")
    suite.tamper("mismatched_hash", mismatched_hash, "function compiledCode hash mismatch")
    save_json(output / "results.json", {"identity": identity, "cases": suite.results})
    failures = sum(not case["passed"] for case in suite.results)
    print(f"{len(suite.results) - failures}/{len(suite.results)} cases passed; {failures} failed", flush=True)
    return 1 if failures else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workspace", type=Path,
                        default=Path(os.environ.get("UAL_WORKSPACE", Path.home() / "ual-workspace")),
                        help="pinned sibling root, not the ledger checkout")
    parser.add_argument("--aiken", type=Path, help="existing CLI binary (default: build this checkout)")
    parser.add_argument("--output", type=Path, help="retain logs/artifacts in this new directory")
    parser.add_argument("--timeout", type=int, default=300, help="per-check wall-clock seconds (default: 300)")
    args = parser.parse_args()
    require(args.timeout > 0, "--timeout must be positive")
    workspace = args.workspace.expanduser().resolve()
    # Reuse the locked setup's clean-source, toolchain, capability and artifact
    # checks. It builds only if this workspace has not been provisioned before.
    subprocess.run([str(ROOT / "scripts/ual/workspace-setup.sh"), "--workspace", str(workspace)], check=True)
    env, tools = preflight(json.loads((SCHEMAS / "assurance-workspace.json").read_text()))
    env["AIKEN_ASSURANCE_WORKSPACE"] = str(workspace / "CardanoLedgerApiBlaster")
    env["NO_COLOR"] = "1"
    if args.aiken:
        aiken = args.aiken.expanduser().resolve()
    else:
        subprocess.run(["cargo", "build", "-p", "aiken"], cwd=ROOT, check=True)
        metadata = json.loads(run(["cargo", "metadata", "--no-deps", "--format-version=1"], ROOT))
        aiken = Path(metadata["target_directory"]) / "debug/aiken"
    require(aiken.is_file(), f"Missing Aiken executable: {aiken}")
    if args.output:
        output = args.output.expanduser().resolve()
        output.mkdir(parents=True, exist_ok=False)
        print(f"Artifacts: {output}", flush=True)
        return execute(args, output, aiken, env, tools, workspace)
    with tempfile.TemporaryDirectory(prefix="aiken-ual-e2e-") as temporary:
        return execute(args, Path(temporary), aiken, env, tools, workspace)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (RuntimeError, OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        print(f"e2e: error: {error}", file=sys.stderr)
        sys.exit(1)
