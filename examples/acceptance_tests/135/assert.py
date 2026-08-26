#!/usr/bin/env python3
import json
import subprocess
import sys
from pathlib import Path

AIKEN = Path(sys.argv[1])
PROJECT = Path(sys.argv[2])
TIMEOUT_SECONDS = 30

CASES = (
    ("verbose", "all", "<expected> Something"),
    ("compact", "all", "Something"),
    ("verbose", "user-defined", "<expected> Something"),
    ("compact", "user-defined", "Something"),
    ("verbose", "compiler-generated", "expect False"),
    ("compact", "compiler-generated", "expect False"),
)

failures = []
for level, trace_filter, expected in CASES:
    command = [
        str(AIKEN),
        "check",
        "-S",
        str(PROJECT),
        "--seed",
        "1",
        "--max-success",
        "1",
        "--trace-level",
        level,
        "--trace-filter",
        trace_filter,
    ]

    try:
        result = subprocess.run(
            command,
            check=False,
            capture_output=True,
            text=True,
            timeout=TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired:
        failures.append(
            f"{level}/{trace_filter}: exceeded {TIMEOUT_SECONDS}s subprocess deadline"
        )
        continue

    if result.returncode != 0:
        failures.append(
            f"{level}/{trace_filter}: command exited {result.returncode}: {result.stderr.strip()}"
        )
        continue

    report = json.loads(result.stdout)
    observed = report["modules"][0]["tests"][0].get("traces", [])
    observed = [
        trace
        for trace in observed
        if trace != "the validator crashed / exited prematurely"
    ]

    if observed != [expected]:
        failures.append(
            f"{level}/{trace_filter}: expected {[expected]!r}, observed {observed!r}"
        )

if failures:
    raise AssertionError(
        "issue #1251 expect-trace matrix diverged:\n  " + "\n  ".join(failures)
    )
