use aiken/collection/dict
use cardano/address.{Address, Script}
use cardano/assets
use cardano/script_context.{ScriptContext, Spending}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptPurpose, Spend, Transaction,
}

validator simple_else {
  else(ctx: ScriptContext) {
    expect Spending(output_ref, _) = ctx.info

    let transaction = ctx.transaction

    assert_transaction_id(transaction.id)

    assert_script_info(output_ref)

    assert_inputs(transaction.inputs)

    expect [] = transaction.outputs

    expect [] = transaction.reference_inputs

    expect [] = transaction.extra_signatories

    expect 42 == transaction.fee

    assert_redeemers(transaction.redeemers)

    expect [] == dict.to_pairs(transaction.datums)

    True
  }
}

fn assert_transaction_id(id: ByteArray) {
  expect
    #"c6fbd346681a8f8337f6b3e51e6ec973f1509367eabc3a44c849af58a1d8471b" == id
  Void
}

fn assert_script_info(info: OutputReference) {
  expect
    OutputReference {
      transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
      output_index: 0,
    } == info
  Void
}

fn assert_inputs(inputs: List<Input>) {
  expect [
    Input {
      output_reference: OutputReference { transaction_id, output_index: 0 },
      output: Output {
        address,
        value: resolved_input_value,
        datum: InlineDatum(_),
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

fn assert_redeemers(redeemers: Pairs<ScriptPurpose, Data>) {
  expect
    [
      Pair(
        Spend(
          OutputReference {
            transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
            output_index: 0,
          },
        ),
        void(),
      ),
    ] == redeemers
  Void
}

fn void() -> Data {
  let void: Data = Void
  void
}
