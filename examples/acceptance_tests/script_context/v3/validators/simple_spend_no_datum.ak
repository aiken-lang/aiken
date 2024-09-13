use aiken/collection/dict
use cardano/address.{Address, Script}
use cardano/assets
use cardano/transaction.{Input, NoDatum, Output, OutputReference, Transaction}

validator simple_spend {
  spend(
    datum: Option<Void>,
    _redeemer: Void,
    _output_ref: OutputReference,
    transaction: Transaction,
  ) {
    expect None = datum

    assert_inputs(transaction.inputs)

    expect [] == dict.to_pairs(transaction.datums)

    True
  }

  else(_ctx) {
    fail
  }
}

fn assert_inputs(inputs: List<Input>) {
  expect [
    Input {
      output_reference: OutputReference { transaction_id, output_index: 0 },
      output: Output {
        address,
        value: resolved_input_value,
        datum: NoDatum,
        reference_script: None,
      },
    },
  ] = inputs

  expect
    transaction_id == #"0000000000000000000000000000000000000000000000000000000000000000"

  expect resolved_input_value == assets.from_lovelace(1000000000)

  expect Address { payment_credential: Script(_), stake_credential: None } =
    address

  Void
}
