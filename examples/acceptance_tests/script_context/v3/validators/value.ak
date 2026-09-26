const my_policy: ByteArray =
  #"00000000000000000000000000000000000000000000000000000000"

const my_asset: ByteArray = "aiken"

validator value {
  spend(
    datum: Option<Value>,
    redeemer: Value,
    _output_ref: Data,
    _transaction: Data,
  ) {
    and {
      (Some(
        {
          lovelace: 42,
          my_policy: {
            "foo": 1,
          },
        },
      ) == datum)?,
      (redeemer == {
        #"11111111111111111111111111111111111111111111111111111111": {
          "foo": 2,
          #"abcd": 1,
        },
        #"00000000000000000000000000000000000000000000000000000000": {
          my_asset: 99,
        },
      })?,
    }
  }

  else(_ctx) {
    fail
  }
}
