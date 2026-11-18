pub type Tree {
  Leaf(Int)
  Branch(Tree, Tree)
}

@onchain(steps: 2500, semantics: c)
pub fn preserve(tree: Tree) -> Tree {
  tree
}

// Constructor 0 with one integer field encodes a well-formed Leaf.
// Quantifying the leaf value avoids claiming preservation of malformed Data.
//@ PROPERTY preserves_leaf for preserve
  "Every well-formed leaf of the recursive tree is returned unchanged."
  ∀ (leaf : Int), fixtures__preserve_returns (Data.Constr 0 [Data.I leaf]) (Data.Constr 0 [Data.I leaf])
@//

// The pinned blueprint schema requires a real validator, outside property scope.
validator companion {
  else(_ctx: Data) {
    fail
  }
}
