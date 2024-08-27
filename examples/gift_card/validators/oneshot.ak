use aiken/collection/dict
use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction} as tx

pub type Action {
  Mint
  Burn
}

validator gift_card(token_name: ByteArray, utxo_ref: OutputReference) {
  mint(rdmr: Action, policy_id: PolicyId, transaction: Transaction) {
    let Transaction { inputs, mint, .. } = transaction

    expect [Pair(asset_name, amount)] =
      mint
        |> assets.tokens(policy_id)
        |> dict.to_pairs()

    when rdmr is {
      Mint -> {
        expect Some(_input) =
          list.find(inputs, fn(input) { input.output_reference == utxo_ref })

        amount == 1 && asset_name == token_name
      }
      Burn -> amount == -1 && asset_name == token_name
    }
  }

  else(_) {
    fail
  }
}

validator redeem(token_name: ByteArray, policy_id: ByteArray) {
  spend(_d, _r, _o_ref: Data, transaction: Transaction) {
    let Transaction { mint, .. } = transaction

    expect [Pair(asset_name, amount)] =
      mint
        |> assets.tokens(policy_id)
        |> dict.to_pairs()

    amount == -1 && asset_name == token_name
  }

  else(_) {
    fail
  }
}
