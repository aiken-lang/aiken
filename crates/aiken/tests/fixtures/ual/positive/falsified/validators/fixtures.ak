@onchain(steps: 100, semantics: e)
pub fn outbids(old: Int, proposed: Int) -> Bool {
  proposed > old
}

// Deliberately false: old = proposed = 0 makes outbids return False, not True.
// Successful producer generation is not evidence that this proposition holds.
//@ PROPERTY equal_bid_outbids for outbids
  "An equal proposed bid returns true (deliberately false)."
  ∀ (old proposed : Int), old = proposed → fixtures__outbids_returns (Data.I old) (Data.I proposed) true
@//

// The pinned blueprint schema requires a real validator, outside property scope.
validator companion {
  else(_ctx: Data) {
    fail
  }
}
