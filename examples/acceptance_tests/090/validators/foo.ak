const foo =
  45898945895546556456546223123184821312385384845153123213185685815615232323484552

validator foo {
  spend(datum: Int, _redeemer: Data, o_ref: Data, _tx: Data) {
    datum == foo
  }
}
