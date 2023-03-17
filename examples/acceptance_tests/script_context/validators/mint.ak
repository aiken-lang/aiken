use aiken/dict
use aiken/list
use aiken/transaction.{Mint, ScriptContext}
use aiken/transaction/value

validator {
  fn mint(redeemer: Data, ctx: ScriptContext) {
    [
      assert_purpose(ctx),
      assert_mint(ctx.purpose, ctx.transaction),
      assert_redeemers(ctx, redeemer),
    ]
    |> list.and
  }
}

fn assert_purpose(ctx) {
  expect [my_policy_id] =
    ctx.transaction.mint
    |> value.without_lovelace
    |> value.policies
  expect Mint(policy_id) = ctx.purpose

  my_policy_id == policy_id
}

fn assert_mint(purpose, transaction) {
  expect Mint(policy_id) = purpose
  let tokens = value.tokens(transaction.mint, policy_id)

  when dict.get(tokens, #"666f6f") is {
    None -> error @"token not found"
    Some(quantity) -> quantity == 1337
  }
}

fn assert_redeemers(ctx, my_redeemer) {
  expect Some(redeemer) = dict.get(ctx.transaction.redeemers, ctx.purpose)
  my_redeemer == redeemer && dict.size(ctx.transaction.redeemers) == 1
}
