pub type Tree {
  Leaf(Int)
  Branch(Tree, Tree)
}

@onchain(steps: 2500, semantics: c)
pub fn preserve(tree: Tree) -> Tree {
  tree
}

//@ PROPERTY preserves_tree for preserve
  "The recursive tree is returned unchanged."
  ∀ (tree : Data), fixtures__preserve_returns tree tree
@//

// Required by the pinned blueprint schema, not by the function property.
validator companion {
  else(_ctx: Data) {
    fail
  }
}
