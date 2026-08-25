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
        f"issue #1359 check exceeded {TIMEOUT_SECONDS}s subprocess deadline"
    ) from error

output = result.stdout + result.stderr
panic_markers = (
    "aiken::fatal::error",
    "Whoops! You found a bug",
    "panicked at",
    "TryFromBigIntError",
)
found_panics = [marker for marker in panic_markers if marker in output]
if found_panics:
    raise AssertionError(
        "issue #1359 runtime panic remained "
        f"({', '.join(found_panics)}):\n{output.strip()}"
    )

try:
    report = json.loads(result.stdout)
except json.JSONDecodeError:
    report = None

if report is not None:
    tests = [test for module in report["modules"] for test in module["tests"]]
    if not any(test["title"] == "issue_1359_reported_datum_does_not_crash" for test in tests):
        raise AssertionError(f"missing coherent issue #1359 test verdict: {result.stdout}")
elif result.returncode != 0 and "aiken::" in output:
    pass
else:
    raise AssertionError(
        "check produced neither a coherent test verdict nor a structured diagnostic:\n"
        + output.strip()
    )
