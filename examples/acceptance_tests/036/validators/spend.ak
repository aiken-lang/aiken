use aiken/dict
use aiken/list
use aiken/transaction.{Output, ScriptContext}
use aiken/transaction/value.{PolicyId}

const my_policy_id: PolicyId = #[0, 0, 0, 0, 0]

pub fn has_policy_id(self: Output, policy_id: PolicyId) -> Bool {
  self.value
  |> value.tokens(policy_id)
  |> dict.is_empty
  |> not
}

pub fn spend(_datum: Data, _redeemer: Data, ctx: ScriptContext) -> Bool {
  ctx.transaction.outputs
  |> list.any(has_policy_id(_, my_policy_id))
}
