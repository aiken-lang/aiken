use example

//@ PREDICATE
def raised (old proposed : Int) : Prop := proofs.helpers.larger old proposed
@//

@onchain(steps: 100, semantics: e)
pub fn outbids(old: Int, proposed: Int) -> Bool {
  example.same(proposed) > old
}

//@ PROPERTY local_helper for outbids
  "An unqualified helper from the property's own module."
  ∀ (old proposed : Int), raised old proposed → fixtures__outbids_returns (Data.I old) (Data.I proposed) true
@//

//@ PROPERTY qualified_import for outbids
  "Helpers imported through an annotation-free bridge, by qualified name."
  ∀ (old proposed : Int), proofs.helpers.larger (proofs.helpers.pick old) proposed → fixtures__outbids_returns (Data.I old) (Data.I proposed) true
@//

//@ PROPERTY escaped_module for outbids
  "A helper of a Lean-keyword module, by escaped qualified name."
  ∀ (old proposed : Int), «example».positive (proposed - old) → fixtures__outbids_returns (Data.I old) (Data.I proposed) true
@//

//@ PROPERTY qualified_falsified for outbids
  "A qualified helper with the wrong direction has a counterexample."
  ∀ (old proposed : Int), proofs.helpers.larger proposed old → fixtures__outbids_returns (Data.I old) (Data.I proposed) true
@//

// The pinned blueprint schema requires a real validator, outside property scope.
validator companion {
  else(_ctx: Data) {
    fail
  }
}
