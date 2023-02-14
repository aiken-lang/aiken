#!/usr/bin/env bash

aiken build
VALIDATOR=$(jq ".validators[] | select(.title == \"basic\")" plutus.json)
