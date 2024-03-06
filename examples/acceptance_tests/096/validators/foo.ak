use aiken/transaction.{ScriptContext}

validator {
  fn is_fourty_two(dat, rdm, _ctx: ScriptContext) -> Bool {
    rdm == dat && #"68656c6c6f20776f726c64" == #"68656c6c6f20776f726c64"
  }
}
