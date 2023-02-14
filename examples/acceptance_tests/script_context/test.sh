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

aiken build
if [ $? -ne 0 ]; then
  exit $?
fi

BLUEPRINT=$(jq ".validators[] | select(.title == \"$TITLE\")" plutus.json)

VALIDATOR_HASH=$(echo $BLUEPRINT | jq .hash | sed s/\"//g)
VALIDATOR=$(echo $BLUEPRINT | jq .compiledCode | sed s/\"//g)
VALIDATOR=$(cbor-diag --to hex --from diag <<< "h'$VALIDATOR'")

cp data/$TITLE/inputs.cbor.template data/$TITLE/inputs.cbor
sed "s/{{ VALIDATOR_HASH }}/$VALIDATOR_HASH/" data/$TITLE/outputs.cbor.template > data/$TITLE/outputs.cbor
sed "s/{{ VALIDATOR }}/$VALIDATOR/" data/$TITLE/tx.cbor.template > data/$TITLE/tx.cbor

cargo run -- tx simulate data/$TITLE/tx.cbor data/$TITLE/inputs.cbor data/$TITLE/outputs.cbor
