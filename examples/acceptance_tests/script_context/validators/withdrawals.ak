use aiken/dict
use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{
  Inline, ScriptCredential, VerificationKeyCredential,
}

fn spend(_datum: Void, _redeemer: Void, ctx: ScriptContext) {
  let alice =
    Inline(
    VerificationKeyCredential(
      #"22222222222222222222222222222222222222222222222222222222",
    ))
    

  let bob =
    Inline(
    ScriptCredential(
      #"5e1e8fa84f2b557ddc362329413caa3fd89a1be26bfd24be05ce0a02",
    ))
    

  [
    when dict.get(ctx.transaction.withdrawals, alice) is {
      None -> error("alice's withdrawal not found")
      Some(value) -> value == 42
    },
    when dict.get(ctx.transaction.withdrawals, bob) is {
      None -> error("bob's withdrawal not found")
      Some(value) -> value == 14
    },
    dict.keys(ctx.transaction.withdrawals) == [alice, bob],
  ]
  |> list.and
}
