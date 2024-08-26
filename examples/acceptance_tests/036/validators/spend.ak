use aiken/dict
use aiken/list
use aiken/transaction.{Output, OutputReference, ScriptContext}
use aiken/transaction/value.{PolicyId}

const my_policy_id: PolicyId = #"0000000000"

pub fn has_policy_id(self: Output, policy_id: PolicyId) -> Bool {
  self.value
    |> value.tokens(policy_id)
    |> dict.is_empty
    |> not
}

validator foo {
  spend(_datum: Data, _redeemer: Data, o_ref: Data, tx: Transaction) -> Bool {
    ctx.transaction.outputs
      |> list.any(has_policy_id(_, my_policy_id))
  }
}

validator bar(output_reference: OutputReference) {
  mint(_redeemer: Void, _policy_id: Data, tx: Transaction) -> Bool {
    when
      list.find(
        ctx.transaction.inputs,
        fn(input) { input.output_reference == output_reference },
      )
    is {
      Some(_) -> True
      None -> False
    }
  }
}
