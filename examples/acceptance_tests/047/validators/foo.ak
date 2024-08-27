validator foo {
  spend(_datum: Option<Void>, _redeemer: Void, oref: Data, _tx: Void) {
    True
  }

  else(_) {
    fail
  }
}

test foo() {
  1 + 1 == 2
}
