validator {
  fn spend(datum: Int, redeemer: Int, _context: Data) {
    datum + redeemer == 42
  }

  fn mint(redeemer: Int, _context: Data) {
    redeemer == 42
  }
}
