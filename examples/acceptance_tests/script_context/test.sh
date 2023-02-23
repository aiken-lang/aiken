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

cargo run --quiet -- build
if [ $? -ne 0 ]; then
  exit $?
fi

BLUEPRINT=$(jq ".validators[] | select(.title|contains(\"$TITLE\"))" plutus.json)

VALIDATOR_HASH=$(echo $BLUEPRINT | jq .hash | sed s/\"//g)
VALIDATOR=$(echo $BLUEPRINT | jq .compiledCode | sed s/\"//g)
VALIDATOR=$(cbor-diag --to hex --from diag <<< "h'$VALIDATOR'")

cp ctx/$TITLE/inputs.cbor.template ctx/$TITLE/inputs.cbor
sed "s/{{ VALIDATOR_HASH }}/$VALIDATOR_HASH/" ctx/$TITLE/outputs.cbor.template > ctx/$TITLE/outputs.cbor
sed "s/{{ VALIDATOR }}/$VALIDATOR/" ctx/$TITLE/tx.cbor.template | sed "s/{{ VALIDATOR_HASH }}/$VALIDATOR_HASH/" > ctx/$TITLE/tx.cbor

cargo run --quiet -- tx simulate ctx/$TITLE/tx.cbor ctx/$TITLE/inputs.cbor ctx/$TITLE/outputs.cbor
