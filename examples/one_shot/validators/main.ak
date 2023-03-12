use aiken/dict
use aiken/list
use aiken/transaction.{OutputReference, ScriptContext, Transaction} as tx
use aiken/transaction/value

type Action {
  Mint
  Burn
}

validator gift_card(token_name: ByteArray, utxo_ref: OutputReference) {
  fn(rdmr: Action, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx

    expect tx.Mint(policy_id) = purpose

    let Transaction { inputs, mint, .. } = transaction

    expect [(asset_name, amount)] =
      mint
      |> value.tokens(policy_id)
      |> dict.to_list()

    when rdmr is {
      Mint -> {
        expect Some(_input) =
          list.find(inputs, fn(input) { input.output_reference == utxo_ref })
        amount == 1 && asset_name == token_name
      }
      Burn -> amount == -1 && asset_name == token_name
    }
  }
}

validator redeem(token_name: ByteArray, policy_id: ByteArray) {
  fn(_d: Void, _r: Void, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, .. } = ctx

    let Transaction { mint, .. } = transaction

    expect [(asset_name, amount)] =
      mint
      |> value.tokens(policy_id)
      |> dict.to_list()

    amount == -1 && asset_name == token_name
  }
}
