use aiken/collection/dict
use aiken/collection/list.{find, foldr}
use cardano/assets.{PolicyId, add, zero}
use cardano/transaction.{Input, OutputReference, Transaction} as tx

pub type Action {
  Mint
  Burn
}

validator foo(token_name: ByteArray, utxo_ref: OutputReference) {
  mint(rdmr: Action, policy_id: PolicyId, transaction: Transaction) {
    let Transaction { inputs, mint, .. } = transaction
    expect [Pair(asset_name, amount)] =
      mint
        |> assets.tokens(policy_id)
        |> dict.to_pairs()
    when rdmr is {
      Mint -> {
        expect
          list.any(inputs, fn(input) { input.output_reference == utxo_ref })
        amount == 1 && asset_name == token_name
      }
      Burn -> todo @"burn"
    }
  }

  else(_) {
    fail
  }
}
