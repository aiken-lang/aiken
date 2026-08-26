#!/usr/bin/env python3
import json
import subprocess
import sys
from pathlib import Path

AIKEN = Path(sys.argv[1])
PROJECT = Path(sys.argv[2])
TIMEOUT_SECONDS = 60

try:
    result = subprocess.run(
        [str(AIKEN), "check", "-S", str(PROJECT), "--seed", "1", "--max-success", "1"],
        check=False,
        capture_output=True,
        text=True,
        timeout=TIMEOUT_SECONDS,
    )
except subprocess.TimeoutExpired as error:
    raise AssertionError(
        f"issue #1314 check exceeded {TIMEOUT_SECONDS}s subprocess deadline"
    ) from error

output = result.stdout + result.stderr
panic_markers = (
    "aiken::fatal::error",
    "Whoops! You found a bug",
    "panicked at",
    "Failed to evaluate constant: EvaluationFailure",
)
found_panics = [marker for marker in panic_markers if marker in output]
if found_panics:
    raise AssertionError(
        "issue #1314 compiler panic remained "
        f"({', '.join(found_panics)}):\n{output.strip()}"
    )

if result.returncode == 0:
    report = json.loads(result.stdout)
    tests = [test for module in report["modules"] for test in module["tests"]]
    matching = [test for test in tests if test["title"] == "use_fail_constant"]
    if len(matching) != 1:
        raise AssertionError(f"missing coherent use_fail_constant verdict: {result.stdout}")
    verdict = matching[0]
    if verdict.get("status") != "pass" or verdict.get("on_failure") != "succeed_eventually":
        raise AssertionError(f"incoherent fail-marked test verdict: {verdict!r}")
else:
    if "aiken::" not in output:
        raise AssertionError(
            "nonzero check result was neither a structured Aiken diagnostic nor a coherent test verdict:\n"
            + output.strip()
        )
