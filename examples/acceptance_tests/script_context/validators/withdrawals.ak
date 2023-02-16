use aiken/dict
use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{
  Inline, ScriptCredential, VerificationKeyCredential,
}

validator spend {
  fn(_datum: Void, _redeemer: Void, ctx: ScriptContext) {
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

    [
      when dict.get(ctx.transaction.withdrawals, alice) is {
        None -> error "alice's withdrawal not found"
        Some(value) -> value == 42
      },
      when dict.get(ctx.transaction.withdrawals, bob) is {
        None -> error "bob's withdrawal not found"
        Some(value) -> value == 14
      },
      dict.keys(ctx.transaction.withdrawals) == [alice, bob],
    ]
    |> list.and
  }
}
