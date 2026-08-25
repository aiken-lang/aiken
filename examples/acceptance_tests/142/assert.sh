#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
AIKEN="$REPO_ROOT/target/release/aiken"

if [ ! -x "$AIKEN" ]; then
  echo "missing release binary: $AIKEN; run 'cargo build --release' first" >&2
  exit 1
fi

python3 "$SCRIPT_DIR/assert.py" "$AIKEN" "$SCRIPT_DIR"
