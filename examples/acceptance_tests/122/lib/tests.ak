const some_pair = Pair(1, 2)

pub type Redeemer =
  Data

pub type Transaction {
  redeemers: Pairs<ByteArray, Redeemer>,
}

pub fn as_data(data: Data) -> Data {
  data
}

pub fn arbitrary_transaction(redeemer: Data) -> Fuzzer<Transaction> {
  fn(prng) { Some((prng, Transaction { redeemers: [Pair("foo", redeemer)] })) }
}

test pierce_list(tx via arbitrary_transaction([42])) fail once {
  expect [redeemer] = tx.redeemers
  redeemer.2nd == as_data([14])
}

test pierce_tuple(tx via arbitrary_transaction((1, 2))) fail once {
  expect [redeemer] = tx.redeemers
  redeemer.2nd == as_data((3, 4))
}

test pierce_pair(tx via arbitrary_transaction(some_pair)) fail once {
  expect [redeemer] = tx.redeemers
  redeemer.2nd == as_data(Pair(3, 4))
}
