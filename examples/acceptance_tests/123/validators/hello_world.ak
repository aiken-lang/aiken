pub type Redeemer =
  ByteArray

validator hello_world {
  spend(datum: Option<Data>, redeemer: Redeemer, _own_ref: Data, _self: Data) {
    trace @"datum": datum
    trace @"redeemer": redeemer
    let must_say_hello = redeemer == "Hello, World!"
    must_say_hello
  }

  else(_) {
    fail
  }
}
