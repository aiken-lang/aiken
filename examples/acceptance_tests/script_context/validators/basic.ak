use aiken/list
use aiken/option
use aiken/transaction.{ScriptContext, Spend, TransactionId}
use aiken/transaction/credential.{VerificationKeyCredential}
use aiken/transaction/value

fn spend(_datum: Void, _redeemer: Void, ctx: ScriptContext) {
  [
    assert_purpose(ctx.purpose),
    assert_outputs(ctx.transaction),
    assert_fee(ctx.transaction),
  ]
  |> list.and
}

fn assert_purpose(purpose) {
  when purpose is {
    Spend(ref) ->
      ref.transaction_id == TransactionId(
        #"0000000000000000000000000000000000000000000000000000000000000000",
      ) && ref.output_index == 0
    _ -> error("script purpose isn't 'Spend'")
  }
}

fn assert_fee(transaction) {
  transaction.fee == value.from_lovelace(42)
}

fn assert_outputs(transaction) {
  when transaction.outputs is {
    [output] ->
      [
        output.value == value.from_lovelace(1000000000),
        output.address.payment_credential == VerificationKeyCredential(
          #"11111111111111111111111111111111111111111111111111111111",
        ),
        option.is_none(output.address.stake_credential),
      ]
      |> list.and
    _ -> error("unexpected number of outputs")
  }
}
