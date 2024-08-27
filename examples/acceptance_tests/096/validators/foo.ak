use cardano/transaction.{Transaction}

validator is_fourty_two {
  spend(dat, rdm, _oref, _ctx: Transaction) {
    Some(rdm) == dat && #"68656c6c6f20776f726c64" == #"68656c6c6f20776f726c64"
  }

  else(_) {
    fail
  }
}
