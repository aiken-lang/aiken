#!/usr/bin/env bash
set -e

cd $(dirname "$0")

cargo run -r -- build -I
