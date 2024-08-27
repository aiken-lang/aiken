validator foo {
  spend(datum: Option<Int>, redeemer: Int, _ref: Data, _transaction: Data) {
    expect Some(datum) = datum
    datum + redeemer == 42
  }

  mint(redeemer: Int, _policy: ByteArray, _transaction: Data) {
    redeemer == 42
  }

  else(_) {
    fail
  }
}
