use aiken/option
use aiken/transaction.{NoDatum,
  ScriptContext, Spend, Transaction, TransactionId}
use aiken/transaction/credential.{VerificationKeyCredential}
use aiken/transaction/value

validator {
  fn spend(_datum: Void, _redeemer: Void, ctx: ScriptContext) {
    and {
      assert_id(ctx.transaction),
      assert_purpose(ctx.purpose),
      assert_outputs(ctx.transaction),
      assert_fee(ctx.transaction),
    }
  }
}

fn assert_id(transaction: Transaction) {
  transaction.id != TransactionId(
    #"0000000000000000000000000000000000000000000000000000000000000000",
  )
}

fn assert_purpose(purpose) {
  when purpose is {
    Spend(ref) ->
      ref.transaction_id == TransactionId(
        #"0000000000000000000000000000000000000000000000000000000000000000",
      ) && ref.output_index == 0
    _ -> fail @"script purpose isn't 'Spend'"
  }
}

fn assert_fee(transaction: Transaction) {
  transaction.fee == value.from_lovelace(42)
}

fn assert_outputs(transaction: Transaction) {
  when transaction.outputs is {
    [output] -> and {
        output.value == value.from_lovelace(1000000000),
        output.address.payment_credential == VerificationKeyCredential(
          #"11111111111111111111111111111111111111111111111111111111",
        ),
        option.is_none(output.address.stake_credential),
        output.datum == NoDatum,
        output.reference_script == None,
      }
    _ -> fail @"unexpected number of outputs"
  }
}
