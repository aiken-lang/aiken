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
    echo -e "\033[1mjq\033[0m missing from system but required."
    exit 1
fi

if ! command -v cbor-diag &> /dev/null
then
    echo -e "\033[1mcbor-diag\033[0m missing from system but required."
    exit 1
fi

$AIKEN build -f all -t verbose
if [ $? -ne 0 ]; then
  exit $?
fi

declare -a VALIDATORS=($(jq -c ".validators | map(select(.title|contains(\"$TITLE\"))) | .[]" plutus.json))

if [ -z $VALIDATORS ]; then
    echo -e "\033[31mvalidator \033[1m$TITLE\033[0m\033[31m not found!\033[0m"
    exit 1
fi

TRANSACTION=$(cat ctx/$TITLE/tx.template)
RESOLVED_INPUTS=$(cat ctx/$TITLE/resolved_inputs.template)

for ITEM in ${VALIDATORS[@]}; do
  VALIDATOR_NAME=$(echo $ITEM | jq -r .title)
  VALIDATOR_HASH=$(echo $ITEM | jq -r .hash)
  VALIDATOR=$(echo $ITEM | jq -r .compiledCode)
  VALIDATOR_CBOR=$(echo "h'$VALIDATOR'" | cbor-diag --to hex --from diag)

  RESOLVED_INPUTS=$(echo $RESOLVED_INPUTS \
    | sed "s/{{ $VALIDATOR_NAME.cbor }}/$VALIDATOR_CBOR/g" \
    | sed "s/{{ $VALIDATOR_NAME.hash }}/$VALIDATOR_HASH/g")

 TRANSACTION=$(echo $TRANSACTION \
   | sed "s/{{ $VALIDATOR_NAME.cbor }}/$VALIDATOR/g" \
   | sed "s/{{ $VALIDATOR_NAME.hash }}/$VALIDATOR_HASH/g")
done

echo $RESOLVED_INPUTS | cbor-diag --to hex --from diag > ctx/$TITLE/resolved_inputs.cbor

echo $TRANSACTION | cbor-diag --to hex --from diag > ctx/$TITLE/tx.cbor

$AIKEN tx simulate \
  ctx/$TITLE/tx.cbor \
  ctx/inputs.cbor \
  ctx/$TITLE/resolved_inputs.cbor
