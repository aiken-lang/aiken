use aiken/list.{find, foldr}
use aiken/transaction.{Input, ScriptContext, Spend, OutputReference, Transaction} as tx
use aiken/transaction/value.{add, zero}
use aiken/dict

 
type Action {
  Mint
  Burn
}
 
validator(token_name: ByteArray, utxo_ref: OutputReference) {
  fn gift_card(rdmr: Action, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } =
      ctx
 
    expect tx.Mint(policy_id) =
      purpose
 
    let Transaction { inputs, mint, .. } =
      transaction
 
    expect [(asset_name, amount)] = mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.to_list()
 
    when rdmr is {
      Mint -> {
        expect True =
          list.any(inputs, fn(input) { input.output_reference == utxo_ref })
        amount == 1 && asset_name == token_name
      }
      Burn ->
        todo @"burn"
    }
  }
}
