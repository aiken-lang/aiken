const foo =
  45898945895546556456546223123184821312385384845153123213185685815615232323484552

validator {
  fn spend(datum: Int, _redeemer: Data, _context: Data) {
    datum == foo
  }
}
