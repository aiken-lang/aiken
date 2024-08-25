use aiken/collection/list
use cardano/transaction.{Output, Transaction}

validator {
  spend(
    _datum: Void,
    _redeemer: Void,
    _ref: Data,
    transaction: Transaction,
  ) -> Bool {
    expect Some(_) = list.find(transaction.outputs, fn(_) { True })
    let _ = find_stuff(transaction)
    True
  }
}

fn find_stuff(transaction: Transaction) -> Output {
  expect Some(stuff) = list.find(transaction.outputs, fn(_) { True })
  stuff
}
