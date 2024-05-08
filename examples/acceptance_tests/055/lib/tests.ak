use aiken/bytearray
use aiken/dict
use aiken/hash.{Hash, Sha2_256, sha2_256}
use aiken/interval.{between, entirely_between, intersection, is_empty}
use aiken/list
use aiken/transaction.{InlineDatum}

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
    Leaf { .. } -> 1
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
  let items =
    [#"aa", #"bb", #"cc"]
  let mt = from_list(items)
  size(mt) == 3
}

test some_test1() {
  InlineDatum(Void) == InlineDatum(Void)
}

test intersection_3() {
  let iv1 = between(0, 1)
  let iv2 = entirely_between(1, 2)
  intersection(iv1, iv2)
    |> is_empty
}

const fooz = #"666f6f"

const bar = #"626172"

fn fixture_1() {
  dict.new()
    |> dict.insert(fooz, 42)
    |> dict.insert(bar, 14)
}

test union_1() {
  dict.union(fixture_1(), dict.new()) == fixture_1()
}
