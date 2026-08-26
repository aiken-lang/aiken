#!/usr/bin/env python3
import os
import signal
import subprocess
import sys

TIMEOUT_SECONDS = float(sys.argv[1])
COMMAND = sys.argv[2:]

if not COMMAND:
    raise SystemExit("usage: run-with-timeout.py SECONDS COMMAND [ARG ...]")

process = subprocess.Popen(COMMAND, start_new_session=True)
try:
    raise SystemExit(process.wait(timeout=TIMEOUT_SECONDS))
except subprocess.TimeoutExpired:
    os.killpg(process.pid, signal.SIGKILL)
    process.wait()
    print(
        f"command exceeded {TIMEOUT_SECONDS:g}s wall-clock deadline: {' '.join(COMMAND)}",
        file=sys.stderr,
    )
    raise SystemExit(124)
