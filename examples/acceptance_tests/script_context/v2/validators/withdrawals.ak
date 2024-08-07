use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{
  Inline, ScriptCredential, VerificationKeyCredential,
}

validator {
  fn spend(_datum: Void, _redeemer: Void, ctx: ScriptContext) {
    let alice =
      Inline(
        VerificationKeyCredential(
          #"22222222222222222222222222222222222222222222222222222222",
        ),
      )

    let bob =
      Inline(
        ScriptCredential(
          #"afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72",
        ),
      )

    and {
      when
        list.find(ctx.transaction.withdrawals, fn(kv) { kv.1st == alice })
      is {
        None -> fail @"alice's withdrawal not found"
        Some(value) -> value.2nd == 42
      },
      when list.find(ctx.transaction.withdrawals, fn(kv) { kv.1st == bob }) is {
        None -> fail @"bob's withdrawal not found"
        Some(value) -> value.2nd == 14
      },
      list.map(ctx.transaction.withdrawals, fn(kv) { kv.1st }) == [alice, bob],
    }
  }
}
