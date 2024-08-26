use aiken/transaction.{Transaction}
use aiken/transaction/credential.{Address}

type TestData {
  addr: Address,
}

type TestDatum {
  data: Option<TestData>,
}

validator validate {
  spend(raw_datum: Option<Data>, _redeemer: Data, oref: Data, _tx: Transaction) -> Bool {
    expect datum: TestDatum = raw_datum
    let TestDatum { data } = datum
    when data is {
      Some(_) -> True
      None -> False
    }
  }
}
