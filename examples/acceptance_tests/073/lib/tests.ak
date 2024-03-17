pub type Params {
  ParamsData { params: Data }
}

pub type MonoDatum {
  ParamsWrapper { params: Params }
}

pub type GlobalDatum {
  price_feed_script_hash: ByteArray,
}

test expect_raw() fail {
  let my_datum: Data =
    GlobalDatum {
      price_feed_script_hash: #"1234567890123456789012345678901234567890",
    }
  expect my_datum_parse: MonoDatum = my_datum
  my_datum_parse == my_datum_parse
}
