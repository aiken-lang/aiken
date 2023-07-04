use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Address}

type TestData {
  addr: Address,
}

type TestDatum {
  data: Option<TestData>,
}

validator {
  fn validate(raw_datum: Data, _redeemer: Data, _context: ScriptContext) -> Bool {
    expect datum: TestDatum = raw_datum
    let TestDatum { data } = datum
    when data is {
      Some(_) -> True
      None -> False
    }
  }
}
