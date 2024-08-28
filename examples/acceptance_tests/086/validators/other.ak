use cardano/address.{Address}
use cardano/transaction.{Transaction}

type TestData {
  addr: Address,
}

type TestDatum {
  data: Option<TestData>,
}

validator validate {
  spend(raw_datum: Option<Data>, _redeemer: Data, oref: Data, _tx: Transaction) {
    expect Some(datum): Option<Data> = raw_datum
    expect datum: TestDatum = datum
    let TestDatum { data } = datum
    when data is {
      Some(_) -> True
      None -> False
    }
  }

  else(_) {
    fail
  }
}
