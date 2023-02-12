use aiken/bytearray
use aiken/dict
use aiken/hash.{Hash, Sha2_256, sha2_256}
use aiken/interval.{Interval, IntervalBound, PositiveInfinity}
use aiken/list
use aiken/string
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, TransactionId,
}
use aiken/transaction/credential.{
  Address, PublicKeyCredential, ScriptCredential, StakeCredential,
}
use aiken/transaction/value

//  MerkleTree in Aiken (ported from: https://github.com/input-output-hk/hydra/blob/master/plutus-merkle-tree/src/Plutus/MerkleTree.hs)

pub type MerkleTree {
  Empty
  Leaf { hash: Hash<Sha2_256, ByteArray>, value: ByteArray }
  Node { hash: Hash<Sha2_256, ByteArray>, left: MerkleTree, right: MerkleTree }
}

pub fn root_hash(t: MerkleTree) -> Hash<Sha2_256, ByteArray> {
  when t is {
    Empty -> #""
    Leaf { hash, .. } -> hash
    Node { hash, .. } -> hash
  }
}

pub fn is_equal(a: MerkleTree, b: MerkleTree) -> Bool {
  root_hash(a) == root_hash(b)
}

pub fn size(t: MerkleTree) -> Int {
  when t is {
    Empty -> 0
    Leaf{..} -> 1
    Node { left, right, .. } -> size(left) + size(right)
  }
}

fn combine_hash(h1: Hash<alg, a>, h2: Hash<alg, a>) -> Hash<alg, a> {
  sha2_256(bytearray.concat(h1, h2))
}

pub fn from_list(items0: List<ByteArray>) -> MerkleTree {
  do_from_list(items0, list.length(items0))
}

fn do_from_list(items: List<ByteArray>, len: Int) -> MerkleTree {
  when items is {
    [] -> Empty
    [value] -> Leaf { hash: sha2_256(value), value }
    all -> {
      let cutoff: Int = len / 2
      let left =
        all
        |> list.take(cutoff)
        |> do_from_list(cutoff)
      let right =
        all
        |> list.drop(cutoff)
        |> do_from_list(len - cutoff)
      let hash = combine_hash(root_hash(left), root_hash(right))
      Node { hash, left, right }
    }
  }
}

test foo() {
  let items = [#"aa", #"bb", #"cc"]
  let mt = from_list(items)
  size(mt) == 3
}

// const keyhash = #"010203040506"

// const scripthash = #"060504030201"

// pub fn keyhash_address(with_stake_credential: Option<StakeCredential>) {
//   Address {
//     payment_credential: PublicKeyCredential(keyhash),
//     stake_credential: with_stake_credential,
//   }
// }

// pub fn scripthash_address(with_stake_credential: Option<StakeCredential>) {
//   Address {
//     payment_credential: ScriptCredential(scripthash),
//     stake_credential: with_stake_credential,
//   }
// }

// type SampleData {
//   a: Int,
//   b: ByteArray,
// }

// pub fn tx_1() -> Transaction {
//   let sample_datum = SampleData { a: 1, b: #"01" }
//   // let sample_data: Data = sample_datum
//   let tx =
//     Transaction {
//       inputs: [
//         Input {
//           output_reference: OutputReference {
//             transaction_id: TransactionId { hash: #"" },
//             output_index: 0,
//           },
//           output: Output {
//             address: scripthash_address(None),
//             value: value.zero(),
//             datum: InlineDatum(sample_datum),
//             reference_script: None,
//           },
//         },
//       ],
//       reference_inputs: [],
//       outputs: [
//         Output {
//           address: keyhash_address(None),
//           value: value.from_lovelace(10000),
//           datum: InlineDatum(sample_datum),
//           reference_script: None,
//         },
//       ],
//       fee: value.zero(),
//       mint: value.from_asset(#"000000", #"00", -1),
//       certificates: [],
//       withdrawals: dict.new(),
//       validity_range: Interval {
//         lower_bound: IntervalBound {
//           bound_type: PositiveInfinity,
//           is_inclusive: True,
//         },
//         upper_bound: IntervalBound {
//           bound_type: PositiveInfinity,
//           is_inclusive: True,
//         },
//       },
//       extra_signatories: [keyhash],
//       redeemers: dict.new(),
//       datums: dict.new(),
//       id: TransactionId { hash: #"" },
//     }
//   tx
// }

// test some_test2() {
//   tx_1() == tx_1()
// }

test some_test1() {
  InlineDatum(Void) == InlineDatum(Void)
}
