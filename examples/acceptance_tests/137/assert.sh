#!/usr/bin/env bash
set -euo pipefail

FIXTURE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$FIXTURE/../../.." && pwd)"
AIKEN="$REPO/target/release/aiken"

if [ ! -x "$AIKEN" ]; then
  echo "missing release binary: $AIKEN; run 'cargo build --release' first" >&2
  exit 1
fi

exec python3 "$FIXTURE/assert.py" --aiken "$AIKEN" --project "$FIXTURE"
