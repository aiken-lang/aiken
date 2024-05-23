use aiken/dict
use aiken/list.{find, foldr}
use aiken/transaction.{Input,
  OutputReference, ScriptContext, Spend, Transaction} as tx
use aiken/transaction/value.{add, zero}

type Action {
  Mint
  Burn
}

validator(token_name: ByteArray, utxo_ref: OutputReference) {
  fn gift_card(rdmr: Action, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect tx.Mint(policy_id) = purpose
    let Transaction { inputs, mint, .. } = transaction
    expect [Pair(asset_name, amount)] =
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
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
}
