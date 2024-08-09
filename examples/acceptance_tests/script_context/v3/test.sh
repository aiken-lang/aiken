#!/usr/bin/env bash

TITLE=$1
if [ -z $TITLE ]; then
  echo -e "\033[31mMissing argument: \033[1mVALIDATOR_TITLE\033[0m"
  echo ""
  echo -e "\033[1mUsage: \033[0m"
  echo "  test.sh {ACCEPTANCE_TEST}"
  echo ""
  echo -e "\033[1mExample: \033[0m"
  echo "  test.sh basic"
  exit 1
fi

AIKEN=${2:-"cargo run -r --quiet --"}

if ! command -v jq &> /dev/null
then
    echo "\033[1mjq\033[0m missing from system but required."
    exit 1
fi

if ! command -v cbor-diag &> /dev/null
then
    echo "\033[1mcbor-diag\033[0m missing from system but required."
    exit 1
fi

$AIKEN build --filter-traces all -t verbose
if [ $? -ne 0 ]; then
  exit $?
fi

BLUEPRINT=$(jq ".validators[] | select(.title|contains(\"$TITLE\"))" plutus.json)

VALIDATOR_HASH=$(echo $BLUEPRINT | jq -r .hash)
VALIDATOR=$(echo $BLUEPRINT | jq -r .compiledCode)

DATUM=$(cbor-diag --to hex --from diag <<< "h'$(cat ctx/$TITLE/datum.cbor)'")

sed "s/{{ VALIDATOR_HASH }}/$VALIDATOR_HASH/" ctx/$TITLE/resolved_inputs.template \
  | sed "s/{{ DATUM }}/$DATUM/" \
  > ctx/$TITLE/resolved_inputs.cbor

sed "s/{{ VALIDATOR }}/$VALIDATOR/" ctx/$TITLE/tx.template \
  | sed "s/{{ VALIDATOR_HASH }}/$VALIDATOR_HASH/" \
  | cbor-diag --to hex --from diag \
  > ctx/$TITLE/tx.cbor

$AIKEN tx simulate \
  ctx/$TITLE/tx.cbor \
  ctx/inputs.cbor \
  ctx/$TITLE/resolved_inputs.cbor
