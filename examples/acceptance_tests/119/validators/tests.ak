pub type Thing =
  Option<Pairs<ByteArray, Pairs<ByteArray, Int>>>

validator foo_3 {
  mint(redeemer: Thing, _policy_, _tx) {
    redeemer == redeemer
  }

  else(_) {
    fail
  }
}
