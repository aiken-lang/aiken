use aiken/collection/list
use cardano/transaction.{Output, Transaction}

validator foo {
  spend(
    _datum: Option<Void>,
    _redeemer: Void,
    _ref: Data,
    transaction: Transaction,
  ) {
    expect Some(_) = list.find(transaction.outputs, fn(_) { True })
    let _ = find_stuff(transaction)
    True
  }

  else(_) {
    fail
  }
}

fn find_stuff(transaction: Transaction) -> Output {
  expect Some(stuff) = list.find(transaction.outputs, fn(_) { True })
  stuff
}
