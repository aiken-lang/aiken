validator {
  fn spend(datum: Int, redeemer: Int, _ref: Data, _transaction: Data) {
    datum + redeemer == 42
  }

  fn mint(redeemer: Int, _policy: ByteArray, _transaction: Data) {
    redeemer == 42
  }
}
