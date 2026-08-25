#!/usr/bin/env python3
import argparse
import json
import re
import subprocess
import tempfile
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("--aiken", type=Path, required=True)
parser.add_argument("--project", type=Path, required=True)
args = parser.parse_args()


def run(command: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, text=True, capture_output=True, check=False)


def export() -> str:
    result = run(
        [
            str(args.aiken),
            "export",
            "--module",
            "export",
            "--name",
            "main",
            str(args.project),
        ]
    )
    if result.returncode != 0:
        print(result.stdout, end="")
        print(result.stderr, end="")
        raise SystemExit(result.returncode)
    try:
        return json.loads(result.stdout)["compiledCode"]
    except (json.JSONDecodeError, KeyError, TypeError) as error:
        print(f"export did not return compiledCode JSON: {error}")
        print(result.stdout, end="")
        raise SystemExit(1) from error


first = export()
second = export()
if first != second:
    print("compiledCode differs across two exports")
    raise SystemExit(1)

with tempfile.TemporaryDirectory(prefix="aiken-issue-1333-") as directory:
    directory = Path(directory)
    cbor_path = directory / "export.cbor"
    cbor_path.write_bytes(bytes.fromhex(first))

    decoded = run([str(args.aiken), "uplc", "decode", "--cbor", str(cbor_path)])
    if decoded.returncode != 0:
        print("fresh-process decode failed")
        print(decoded.stdout, end="")
        print(decoded.stderr, end="")
        raise SystemExit(decoded.returncode)

    match = re.fullmatch(
        r"\s*\(program\s+([0-9]+\.[0-9]+\.[0-9]+)\s+(.*)\)\s*",
        decoded.stdout,
        re.DOTALL,
    )
    if match is None:
        print("decoded artifact is not a textual UPLC program")
        print(decoded.stdout, end="")
        raise SystemExit(1)

    applied_path = directory / "applied.uplc"
    applied_path.write_text(
        f"(program {match.group(1)} [{match.group(2)} (con data (I 1))])\n",
        encoding="utf-8",
    )
    evaluated = run([str(args.aiken), "uplc", "eval", str(applied_path)])
    print(evaluated.stdout, end="")
    print(evaluated.stderr, end="")
    if evaluated.returncode != 0 or "True" not in evaluated.stdout:
        print("exported program could not be evaluated independently as main(1) == True")
        raise SystemExit(1)

print("export is deterministic, independently decodable, closed on the reachable path, and evaluable")
