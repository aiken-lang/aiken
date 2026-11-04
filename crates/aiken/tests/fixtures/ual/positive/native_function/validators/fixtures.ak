//@ PREDICATE
def larger (old proposed : Int) : Prop := old < proposed
@//

@onchain(steps: 100, semantics: e)
pub fn outbids(old: Int, proposed: Int) -> Bool {
  proposed > old
}

//@ PROPERTY outbids for outbids
  "A larger proposed bid returns true."
  ∀ (old proposed : Int), larger old proposed → fixtures__outbids_returns (Data.I old) (Data.I proposed) true
@//

// The pinned blueprint schema requires a real validator, outside property scope.
validator companion {
  else(_ctx: Data) {
    fail
  }
}
