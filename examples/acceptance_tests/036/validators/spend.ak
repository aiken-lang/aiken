use aiken/collection/dict
use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{Output, OutputReference, Transaction}

const my_policy_id: PolicyId = #"0000000000"

pub fn has_policy_id(self: Output, policy_id: PolicyId) -> Bool {
  self.value
    |> assets.tokens(policy_id)
    |> dict.is_empty
    |> not
}

validator foo {
  spend(_datum: Option<Data>, _redeemer: Data, _o_ref: Data, self: Transaction) {
    self.outputs
      |> list.any(has_policy_id(_, my_policy_id))
  }

  else(_) {
    fail
  }
}

validator bar(output_reference: OutputReference) {
  mint(_redeemer: Void, _policy_id, self: Transaction) {
    when
      list.find(
        self.inputs,
        fn(input) { input.output_reference == output_reference },
      )
    is {
      Some(_) -> True
      None -> False
    }
  }

  else(_) {
    fail
  }
}
