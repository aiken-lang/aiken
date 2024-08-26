use aiken/transaction.{ScriptContext}

validator is_fourty_two {
  spend(dat, rdm, oref, _ctx: Transaction) -> Bool {
    rdm == dat && #"68656c6c6f20776f726c64" == #"68656c6c6f20776f726c64"
  }
}
