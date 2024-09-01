use aiken/collection/list
use aiken/crypto.{Hash, Sha2_256, sha2_256}
use aiken/option.{choice, is_none}
use aiken/primitive/bytearray.{from_string}

/// Variant of MerkleTree with only hash but without actual value
pub type MerkleTree<a> {
  Empty
  Leaf { hash: Hash<Sha2_256, ByteArray> }
  Node {
    hash: Hash<Sha2_256, ByteArray>,
    left: MerkleTree<a>,
    right: MerkleTree<a>,
  }
}

pub type Proof =
  List<ProofItem>

pub type ProofItem {
  Left { hash: Hash<Sha2_256, ByteArray> }
  Right { hash: Hash<Sha2_256, ByteArray> }
}

// Function returning a hash of a given Merkle Tree element
pub fn root_hash(self: MerkleTree<a>) -> Hash<Sha2_256, ByteArray> {
  when self is {
    Empty -> #""
    Leaf { hash } -> hash
    Node { hash, .. } -> hash
  }
}

/// Function atests whether two Merkle Tress are equal, this is the case when their root hashes match.
pub fn is_equal(left: MerkleTree<a>, right: MerkleTree<a>) -> Bool {
  root_hash(left) == root_hash(right)
}

/// Function returns a total numbers of leaves in the tree.
pub fn size(self: MerkleTree<a>) -> Int {
  when self is {
    Empty -> 0
    Leaf { .. } -> 1
    Node { left, right, .. } -> size(left) + size(right)
  }
}

fn combine_hash(
  left: Hash<Sha2_256, a>,
  right: Hash<Sha2_256, a>,
) -> Hash<Sha2_256, a> {
  sha2_256(bytearray.concat(left, right))
}

/// Function that returns whether merkle tree has any elements
pub fn is_empty(self: MerkleTree<a>) -> Bool {
  when self is {
    Empty -> True
    _ -> False
  }
}

fn do_proof(
  self: MerkleTree<a>,
  item_hash: Hash<Sha2_256, ByteArray>,
  proof: Proof,
  serialise_fn: fn(a) -> ByteArray,
) -> Option<Proof> {
  when self is {
    Empty -> None
    Leaf { hash } ->
      if hash == item_hash {
        Some(proof)
      } else {
        None
      }
    Node { left, right, .. } -> {
      let rh = root_hash(right)
      let lh = root_hash(left)
      let go_left: Option<Proof> =
        do_proof(
          left,
          item_hash,
          list.push(proof, Right { hash: rh }),
          serialise_fn,
        )
      let go_right: Option<Proof> =
        do_proof(
          right,
          item_hash,
          list.push(proof, Left { hash: lh }),
          serialise_fn,
        )
      choice([go_left, go_right])
    }
  }
}

/// Construct a membership 'Proof' from an element and a 'MerkleTree'. Returns
/// 'None' if the element isn't a member of the tree to begin with.
/// Note function will return Some([]) in case root of the tree is also it's only one and only element
pub fn get_proof(
  self: MerkleTree<a>,
  item: a,
  serialise_fn: fn(a) -> ByteArray,
) -> Option<Proof> {
  let empty: Proof =
    []

  do_proof(self, sha2_256(serialise_fn(item)), empty, serialise_fn)
}

fn do_from_list(
  items: List<a>,
  len: Int,
  serialise_fn: fn(a) -> ByteArray,
) -> MerkleTree<a> {
  when items is {
    [] -> Empty
    [item] -> {
      let hashed_item = sha2_256(serialise_fn(item))
      Leaf { hash: hashed_item }
    }
    all -> {
      let cutoff: Int = len / 2
      let left =
        all
          |> list.take(cutoff)
          |> do_from_list(cutoff, serialise_fn)
      let right =
        all
          |> list.drop(cutoff)
          |> do_from_list(len - cutoff, serialise_fn)
      let hash = combine_hash(root_hash(left), root_hash(right))
      Node { hash, left, right }
    }
  }
}

/// Construct a 'MerkleTree' from a list of elements.
/// Note that, while this operation is doable on-chain, it is expensive and
/// preferably done off-chain.
pub fn from_list(
  items: List<a>,
  serialise_fn: fn(a) -> ByteArray,
) -> MerkleTree<a> {
  do_from_list(items, list.length(items), serialise_fn)
}

fn do_from_hashes_list(
  items: List<Hash<Sha2_256, a>>,
  len: Int,
) -> MerkleTree<a> {
  when items is {
    [] -> Empty
    [hashed_item] -> Leaf { hash: hashed_item }
    all -> {
      let cutoff: Int = len / 2
      let left =
        all
          |> list.take(cutoff)
          |> do_from_hashes_list(cutoff)
      let right =
        all
          |> list.drop(cutoff)
          |> do_from_hashes_list(len - cutoff)
      let hash = combine_hash(root_hash(left), root_hash(right))
      Node { hash, left, right }
    }
  }
}

/// Construct a 'MerkleTree' from a list of hashes.
/// Note that, while this operation is doable on-chain, it is expensive and
/// preferably done off-chain.
pub fn from_hashes_list(items: List<Hash<Sha2_256, a>>) -> MerkleTree<a> {
  do_from_hashes_list(items, list.length(items))
}

// Check whether a hashed element is part of a 'MerkleTree' using only its root hash
// and a 'Proof'. The proof is guaranteed to be in log(n) of the size of the
// tree, which is why we are interested in such data-structure in the first
// place.
pub fn member_from_hash(
  item_hash: Hash<Sha2_256, a>,
  root_hash: Hash<Sha2_256, a>,
  proof: Proof,
  serialise_fn: fn(a) -> ByteArray,
) -> Bool {
  when proof is {
    [] -> root_hash == item_hash
    [head, ..tail] ->
      when head is {
        Left { hash: l } ->
          member_from_hash(
            combine_hash(l, item_hash),
            root_hash,
            tail,
            serialise_fn,
          )
        Right { hash: r } ->
          member_from_hash(
            combine_hash(item_hash, r),
            root_hash,
            tail,
            serialise_fn,
          )
      }
  }
}

// Check whether an element is part of a 'MerkleTree' using only its root hash
// and a 'Proof'.
pub fn member(
  item: a,
  root_hash: Hash<Sha2_256, ByteArray>,
  proof: Proof,
  serialise_fn: fn(a) -> ByteArray,
) -> Bool {
  let item_hash = sha2_256(serialise_fn(item))
  member_from_hash(item_hash, root_hash, proof, serialise_fn)
}

pub fn member_from_tree(
  tree: MerkleTree<a>,
  item: a,
  serialise_fn: fn(a) -> ByteArray,
) -> Bool {
  let proof: Option<Proof> = get_proof(tree, item, serialise_fn)
  let rh = root_hash(tree)

  when proof is {
    Some(p) -> member(item, rh, p, serialise_fn)
    None -> False
  }
}

// needed only for tests
fn create_string_item_serialise_fn() -> fn(String) -> ByteArray {
  fn(x: String) { from_string(x) }
}

test from_hashes_list_5() {
  let dog = @"dog"
  let cat = @"cat"
  let mouse = @"mouse"
  let horse = @"horse"

  let serialise_fn = create_string_item_serialise_fn()

  let items =
    [dog, cat, mouse, horse]
  let hashes_items = list.map(items, fn(item) { sha2_256(serialise_fn(item)) })

  let mt = from_hashes_list(hashes_items)

  let left_node_hash =
    sha2_256(
      bytearray.concat(sha2_256(serialise_fn(dog)), sha2_256(serialise_fn(cat))),
    )
  let right_node_hash =
    sha2_256(
      bytearray.concat(
        sha2_256(serialise_fn(mouse)),
        sha2_256(serialise_fn(horse)),
      ),
    )

  let root_hash = sha2_256(bytearray.concat(left_node_hash, right_node_hash))

  Node {
    hash: root_hash,
    left: Node {
      hash: left_node_hash,
      left: Leaf { hash: sha2_256(serialise_fn(dog)) },
      right: Leaf { hash: sha2_256(serialise_fn(cat)) },
    },
    right: Node {
      hash: right_node_hash,
      left: Leaf { hash: sha2_256(serialise_fn(mouse)) },
      right: Leaf { hash: sha2_256(serialise_fn(horse)) },
    },
  } == mt
}
