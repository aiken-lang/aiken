#!/usr/bin/env bash
set -e

cd "$(dirname "$0")"

cargo run -r -- check \
  --seed 1 \
  --max-success 1 \
  --trace-level verbose \
  --trace-filter compiler-generated \
  .
