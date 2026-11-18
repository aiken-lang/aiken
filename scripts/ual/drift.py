#!/usr/bin/env python3
"""Compare public UAL upstreams with the consumer pins; never change a pin."""

import argparse
from datetime import datetime, timezone
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import urllib.request

from drift_fixtures import validate_fixtures

ROOT = Path(__file__).resolve().parents[2]
REPOS = ("UniversalAnnotationLanguage", "PlutusCoreBlaster", "CardanoLedgerApiBlaster")
TRACKED_PRS = {"PlutusCoreBlaster": (69, 70, 74),
               "UniversalAnnotationLanguage": (3,), "CardanoLedgerApiBlaster": (14,)}
ENCODING = "PlutusCore/UPLC/BlueprintEncoding/"
ASSURANCE = ENCODING + "Assurance.lean"
CONTEXTS = "CardanoLedgerApi/V3/Contexts.lean"
EXIT_CODES = {"no-drift": 0, "re-pin-only": 10, "needs-work": 20}


def run(command, cwd=None):
    result = subprocess.run(command, cwd=cwd, capture_output=True,
                            timeout=300, env={**os.environ, "GIT_TERMINAL_PROMPT": "0"})
    if result.returncode:
        raise RuntimeError(f"{command}: exit {result.returncode}\n"
                           + (result.stdout + result.stderr).decode(errors="replace"))
    return result.stdout.decode()


def save(path, value):
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False) + "\n")


class Source:
    def __init__(self, name, pin, args, output):
        self.pin = pin
        self.path = args.sources / name if args.sources else output / "sources" / name
        if not args.sources:
            self.path.mkdir(parents=True)
            run(["git", "init", "--bare", "--quiet", str(self.path)])
        if not args.offline:
            url = "https://github.com/input-output-hk/" + name
            run(["git", "fetch", "--depth=1", url, pin], self.path)
            if not args.at_pins:
                run(["git", "fetch", "--depth=1", url, "HEAD"], self.path)
        # Explicit pin mode does not inspect dirty upstream worktrees. Offline
        # candidate mode does, so simulations need no commits or network access.
        self.head = pin if args.at_pins else run(
            ["git", "rev-parse", "HEAD" if args.offline else "FETCH_HEAD"], self.path).strip()
        self.worktree = args.offline and not args.at_pins
        if self.worktree and run(["git", "rev-parse", "--is-bare-repository"], self.path).strip() == "true":
            raise RuntimeError("Offline candidate sources must be working checkouts")
        run(["git", "cat-file", "-e", pin + "^{commit}"], self.path)

    def read(self, path, baseline=False):
        if self.worktree and not baseline:
            file = self.path / path
            return file.read_bytes().decode() if file.is_file() else None
        revision = self.pin if baseline else self.head
        files = run(["git", "ls-tree", "--name-only", revision, "--", path], self.path).splitlines()
        if path not in files:
            return None
        return run(["git", "show", revision + ":" + path], self.path)

    def changes(self):
        command = ["git", "diff", "--name-only", self.pin]
        if not self.worktree:
            command.append(self.head)
        names = run(command + ["--"], self.path).splitlines()
        if self.worktree:
            names += run(["git", "ls-files", "--others", "--exclude-standard"], self.path).splitlines()
        return sorted(set(names))


def verdicts(source):
    if source is None:
        raise RuntimeError("Missing Assurance.lean verdict source")
    # Keep all property/summary literals, including warnings and failures. The
    # full source is compared too: moved emitters or renamed prefixes fail closed.
    messages = re.findall(r'"((?:Property |Assurance )(?:[^"\\]|\\.)*)"', source)
    if not messages or not any("failed/inconclusive" in s for s in messages):
        raise RuntimeError("Unrecognised #verify_blueprint verdict protocol")
    return "\n".join(messages) + "\n"


def constructors(source):
    if source is None:
        raise RuntimeError("Missing V3 Contexts.lean")
    match = re.search(r"^inductive ScriptInfo where\s*\n(.*?)(?=^\S)", source,
                      re.MULTILINE | re.DOTALL)
    if not match or "|" not in match[1]:
        raise RuntimeError("Unrecognised V3 ScriptInfo declaration")
    # Preserve argument types and ordering, not only names: both affect lowering.
    return match[0]


def compare(report, output, kind, name, before, after):
    if before == after:
        return
    index = len(report["changes"])
    diff = "".join(difflib.unified_diff(
        (before or "").splitlines(keepends=True), (after or "").splitlines(keepends=True),
        fromfile="pin/" + name, tofile="candidate/" + name))
    path = f"diffs/{index:03d}-{kind}.diff"
    (output / path).write_text(diff)
    report["changes"].append({"kind": kind, "path": name, "diff": path,
                              "beforeSha256": digest(before), "afterSha256": digest(after)})


def digest(text):
    return hashlib.sha256(text.encode()).hexdigest() if text is not None else None


def check_contracts(args, output, report):
    schemas = args.schemas
    pin = dict(line.split(": ", 1) for line in (schemas / "PIN").read_text().splitlines() if ": " in line)
    lock = json.loads((schemas / "assurance-workspace.json").read_text())
    pins = {"PlutusCoreBlaster": pin["commit"],
            "UniversalAnnotationLanguage": lock["repositories"]["ual"]["commit"],
            "CardanoLedgerApiBlaster": lock["repositories"]["ledger"]["commit"]}
    sources = {}
    for name in REPOS:
        source = Source(name, pins[name], args, output)
        sources[name] = source
        report["upstreams"][name] = {"pin": source.pin, "head": source.head,
                                    "workingTree": source.worktree, "changedFiles": source.changes()}
    core = sources["PlutusCoreBlaster"]
    paths = [line for line in (schemas / "PIN").read_text().splitlines()
             if line.startswith((ENCODING, "scripts/"))]
    if not paths or "scripts/assurance-workspace.json" not in paths:
        raise RuntimeError("PIN has no complete upstream file inventory")
    for path in paths:
        before = core.read(path, baseline=True)
        if before is None:
            raise RuntimeError(f"Pinned upstream file is missing: {path}")
        local = schemas / Path(path).name
        compare(report, output, "vendored", path, before,
                local.read_bytes().decode() if local.is_file() else None)
        compare(report, output, "workspace" if path.startswith("scripts/") else "schema",
                path, before, core.read(path))
    before, after = core.read(ASSURANCE, True), core.read(ASSURANCE)
    compare(report, output, "verdicts", ASSURANCE, verdicts(before), verdicts(after))
    compare(report, output, "consumer-source", ASSURANCE, before, after)
    ledger = sources["CardanoLedgerApiBlaster"]
    compare(report, output, "script-info", CONTEXTS,
            constructors(ledger.read(CONTEXTS, True)), constructors(ledger.read(CONTEXTS)))
    ual = sources["UniversalAnnotationLanguage"]
    for path in report["upstreams"]["UniversalAnnotationLanguage"]["changedFiles"]:
        compare(report, output, "ual", path, ual.read(path, True), ual.read(path))
    # UAL-12 deliberately registers the exact consumer revision. Even identical
    # messages at a new revision require a deliberate reader registration change.
    if core.head != core.pin:
        report["workItems"].append("Review/register the new consumer revision in PinnedVerdictReader before re-pinning.")


def check_prs(args, report):
    if args.at_pins:
        report["pullRequests"] = {"status": "skipped", "reason": "Pin baseline mode; no live PR observation"}
        return
    if args.offline and not args.pr_state:
        report["pullRequests"] = {"status": "skipped", "reason": "Offline simulation; supply --pr-state to replay PR observations"}
        return
    supplied = json.loads(args.pr_state.read_text()) if args.pr_state else None
    previous = None
    if args.previous:
        previous = json.loads(args.previous.read_text())["pullRequests"]
        if previous.get("status") != "passed":
            raise RuntimeError("Previous report has no completed PR observation")
        previous = previous["items"]
    items = {}
    for repo, numbers in TRACKED_PRS.items():
        for number in numbers:
            key = f"{repo}#{number}"
            if supplied is not None:
                current = supplied[key]
            else:
                url = f"https://api.github.com/repos/input-output-hk/{repo}/pulls/{number}"
                headers = {"Accept": "application/vnd.github+json", "User-Agent": "aiken-ual-drift"}
                if os.environ.get("GITHUB_TOKEN"):
                    headers["Authorization"] = "Bearer " + os.environ["GITHUB_TOKEN"]
                with urllib.request.urlopen(urllib.request.Request(url, headers=headers), timeout=60) as response:
                    data = json.load(response)
                current = {"state": "merged" if data["merged_at"] else data["state"], "draft": data["draft"]}
            if current.get("state") not in ("open", "closed", "merged") or not isinstance(current.get("draft"), bool):
                raise RuntimeError(f"Invalid PR state for {key}")
            baseline = (previous[key]["current"] if previous else
                        {"state": "open", "draft": repo == "CardanoLedgerApiBlaster"})
            items[key] = {"url": f"https://github.com/input-output-hk/{repo}/pull/{number}",
                          "previous": baseline, "current": current, "changed": baseline != current}
            if baseline != current:
                report["workItems"].append(f"Review upstream PR transition {key}: {baseline} -> {current}")
    report["pullRequests"] = {"status": "passed", "baseline": "previous-report" if previous else "plan-2026-11-03",
                              "items": items}


def check_e2e(args, output, report):
    script = ROOT / "scripts/ual/e2e.sh"
    if not script.is_file():
        report["e2e"] = {"status": "skipped", "reason": "UAL-13 script is absent"}
        return
    ledger = args.workspace / "CardanoLedgerApiBlaster"
    if not ledger.exists():
        report["e2e"] = {"status": "skipped", "reason": "No consumer at " + str(args.workspace)}
        return
    if not (ledger / "lakefile.lean").is_file():
        raise RuntimeError(f"Consumer checkout exists but has no lakefile.lean: {ledger}")
    command = [str(script), "--workspace", str(args.workspace), "--output", str(output / "e2e")]
    if args.aiken:
        command += ["--aiken", str(args.aiken)]
    # e2e enforces pinned revisions, installed toolchain and recfun capability.
    # It must never silently build or validate a newly fetched upstream head.
    with (output / "e2e.log").open("w") as log:
        result = subprocess.run(command, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT)
    report["e2e"] = {"status": "passed" if result.returncode == 0 else "failed",
                     "revisionScope": "pinned consumer, not candidate heads", "command": command,
                     "exitCode": result.returncode, "log": "e2e.log"}
    if result.returncode:
        raise RuntimeError("Pinned consumer e2e failed; see e2e.log")


def outcome(report):
    if report["errors"] or report["changes"] or report["workItems"]:
        return "needs-work"
    if any(s["pin"] != s["head"] or s["changedFiles"] for s in report["upstreams"].values()):
        return "re-pin-only"
    return "no-drift"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True, help="new report/artifact directory")
    parser.add_argument("--sources", type=Path, help="existing public checkouts containing pinned objects")
    parser.add_argument("--offline", action="store_true", help="inspect local source worktrees; never fetch or query GitHub")
    parser.add_argument("--at-pins", action="store_true", help="compare immutable pin snapshots, not upstream heads or PRs")
    parser.add_argument("--schemas", type=Path, default=ROOT / "crates/aiken-ual/schemas")
    parser.add_argument("--previous", type=Path, help="previous successful JSON report for PR transitions")
    parser.add_argument("--pr-state", type=Path, help="offline PR observations keyed by Repository#number")
    parser.add_argument("--aiken", type=Path, help="existing CLI; default builds this checkout")
    parser.add_argument("--workspace", type=Path,
                        default=Path(os.environ.get("UAL_WORKSPACE", Path.home() / "ual-workspace")))
    args = parser.parse_args()
    for name in ("output", "sources", "schemas", "previous", "pr_state", "aiken", "workspace"):
        if getattr(args, name) is not None:
            setattr(args, name, getattr(args, name).expanduser().resolve())
    output = args.output
    output.mkdir(parents=True, exist_ok=False)
    (output / "diffs").mkdir()
    report = {"format": "aiken-ual-drift-v1", "observedAt": datetime.now(timezone.utc).isoformat(),
              "mode": "pins" if args.at_pins else "offline" if args.offline else "live",
              "upstreams": {}, "changes": [], "workItems": [], "errors": [],
              "workerPrompt": "scripts/ual/drift-worker-prompt.md"}
    checks = [
        ("contracts", lambda: check_contracts(args, output, report)),
        ("pullRequests", lambda: check_prs(args, report)),
        ("fixtures", lambda: report.update(fixtures=validate_fixtures(ROOT, output / "fixtures", args.aiken))),
        ("e2e", lambda: check_e2e(args, output, report)),
    ]
    for name, check in checks:
        try:
            if name == "contracts" and args.offline and not args.sources:
                raise RuntimeError("--offline requires --sources")
            check()
        except (RuntimeError, OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
            report["errors"].append({"check": name, "message": str(error)})
            report.setdefault(name, {"status": "failed"})
    report["outcome"] = outcome(report)
    report["exitCode"] = EXIT_CODES[report["outcome"]]
    summary = [f"UAL drift: {report['outcome']} (exit {report['exitCode']}; {report['mode']})",
               f"{len(report['changes'])} contract diffs; {len(report['workItems'])} work items; {len(report['errors'])} errors."]
    summary += [f"{name}: {value.get('status', 'unknown')}" +
                (f" — {value['reason']}" if "reason" in value else "")
                for name in ("fixtures", "e2e", "pullRequests") if (value := report.get(name))]
    summary += report["workItems"]
    summary += [f"{e['check']}: {e['message']}" for e in report["errors"]]
    summary += ["Candidate runtimes were not built or verified; re-pin-only is a static triage result.",
                "Report: " + str(output / "report.json")]
    save(output / "report.json", report)
    (output / "summary.txt").write_text("\n".join(summary) + "\n")
    print("\n".join(summary))
    return report["exitCode"]


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError) as error:
        print(f"drift: cannot create report: {error}", file=sys.stderr)
        sys.exit(20)
