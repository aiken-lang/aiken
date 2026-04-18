use aiken/cbor
use aiken/collection/dict
use aiken/crypto.{blake2b_256}
use cardano/transaction

validator datum_hash {
  spend(
    _datum: Option<Data>,
    _redeemer: Void,
    _output_ref: Data,
    transaction: transaction.Transaction,
  ) {
    expect [Pair(key, value)] = dict.to_pairs(transaction.datums)

    expect value == as_data(Void)

    let value_bytes = cbor.serialise(value)
    let void_bytes = cbor.serialise(Void)
    let value_hash = blake2b_256(value_bytes)

    trace @"datum (from context)": value_bytes
    trace @"datum (re-computed)": void_bytes

    trace @"datum hash (from context)": key
    trace @"datum hash (re-computed)": value_hash

    and {
      key == #"1b44214718f87314c393e4f1cd32b06bc7df47cf60925fe1ea7393551c170c85",
      value_hash == #"923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
    }
  }

  else(_ctx) {
    fail
  }
}
// validator datum_hash {
//   spend(
//     _datum: Option<Data>,
//     _redeemer: Void,
//     _output_ref: Data,
//     transaction: transaction.Transaction,
//   ) {
//     expect [Pair(key, value)] = dict.to_pairs(transaction.datums)
//
//     let value_bytes = cbor.serialise(value)
//
//     trace @"datum hash (from context)": key
//     trace @"datum (from context)": value_bytes
//     trace @"datum hash (re-computed)": blake2b_256(value_bytes)
//
//     and {
//       key == #"bd0e6799921e386a18c7b61f16f0d6ddf76d255cdc7cfb740233cca26571fd4a",
//       blake2b_256(value_bytes) == #"",
//     }
//   }
//
//   else(_ctx) {
//     fail
//   }
// }
