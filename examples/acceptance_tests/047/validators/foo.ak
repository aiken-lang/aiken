validator foo {
  spend(_datum: Void, _redeemer: Void, oref: Data, _tx: Void) -> Bool {
    True
  }
}

test foo() {
  1 + 1 == 2
}
