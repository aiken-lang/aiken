use aiken/dict
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Spending,
}
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value

validator {
  fn mint_1(_tmp2: Void, ctx: ScriptContext) {
    True
  }
}

validator {
  fn mint_2(_tmp2: Void, ctx: ScriptContext) {
    True
  }
}
