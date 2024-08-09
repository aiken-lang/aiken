use aiken/dict
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Spending,
}
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value

validator {
  fn spend(_tmp1: Void, _tmp2: Void, ctx: ScriptContext) {
    let void: Data = Void

    expect
      #"78ec148ea647cf9969446891af31939c5d57b275a2455706782c6183ef0b62f1" == ctx.transaction.id

    expect
      Spending(
        OutputReference {
          transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
          output_index: 0,
        },
        Some(void),
      ) == ctx.info

    trace @"inputs": ctx.transaction.inputs

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
    ] = ctx.transaction.inputs

    expect
      transaction_id == #"0000000000000000000000000000000000000000000000000000000000000000"

    expect resolved_input_value == value.from_lovelace(1000000000)

    expect Address {
      payment_credential: ScriptCredential(_),
      stake_credential: None,
    } = address

    expect [] = ctx.transaction.reference_inputs

    expect [] = ctx.transaction.extra_signatories

    expect 42 == ctx.transaction.fee

    expect
      [
        Pair(Spend(
          OutputReference {
            transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
            output_index: 0,
          },
        ),
        void),
      ] == ctx.transaction.redeemers

    expect [] == dict.to_pairs(ctx.transaction.datums)

    True
  }
}
