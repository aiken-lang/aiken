use aiken/dict
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
      when dict.get(ctx.transaction.withdrawals, alice) is {
        None -> fail @"alice's withdrawal not found"
        Some(value) -> value == 42
      },
      when dict.get(ctx.transaction.withdrawals, bob) is {
        None -> fail @"bob's withdrawal not found"
        Some(value) -> value == 14
      },
      dict.keys(ctx.transaction.withdrawals) == [alice, bob],
    }
  }
}
