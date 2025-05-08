#!/usr/bin/env bash
set -e

cd $(dirname "$0")

cargo run -r -- build -t verbose

FROM=122171C7C82348C420A47AFD8FE87730C913CEEC22524B98E1604A84
TO=$(jq -r ".validators[0].hash" plutus.json)

JSON=$(cargo run -r -- tx simulate tx.cbor inputs.cbor outputs.cbor)

if [ "$(jq -r ".[0].traces" <<< $JSON)" != "[]" ]; then
  echo "❌ expected no traces in initial transaction"
  exit 1
fi

JSON=$(cargo run -r -- tx simulate tx.cbor inputs.cbor outputs.cbor --script-override "$FROM:$TO")

if [ "$(jq -r ".[0].traces[0]" <<< $JSON)" != "datum: 122([])" ]; then
  echo "❌ missing or unexpected datum trace"
  exit 1
fi

if [ "$(jq -r ".[0].traces[1]" <<< $JSON)" != "redeemer: h'48656C6C6F2C20576F726C6421'" ]; then
  echo "❌ missing or unexpected redeemer trace"
  exit 1
fi
