#!/usr/bin/env bash
set -e

cd $(dirname "$0")

cargo run -r -- build
JSON=$(cargo run -r -- blueprint apply 43666F6F)

if [ $(jq -r ".validators[0].title" <<< $JSON) != "tests.my_script.mint" ]; then
  echo "❌ invalid mint handler name"
  exit 1
fi

if [ $(jq -r ".validators[1].title" <<< $JSON) != "tests.my_script.spend" ]; then
  echo "❌ invalid spend handler name"
  exit 1
fi

if [ $(jq -r ".validators[2].title" <<< $JSON) != "tests.my_script.else" ]; then
  echo "❌ invalid else handler name"
  exit 1
fi

if [ $(jq -r ".validators[0].hash" <<< $JSON) != $(jq -r ".validators[1].hash" <<< $JSON) ]; then
  echo "❌ hash mismatch between mint and spend handlers"
  exit 1
fi

if [ $(jq -r ".validators[1].hash" <<< $JSON) != $(jq -r ".validators[2].hash" <<< $JSON) ]; then
  echo "❌ hash mismatch between spend and else handlers"
  exit 1
fi
