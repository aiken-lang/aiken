@onchain(steps: 100, semantics: e)
pub fn f(value: Int) -> Int {
  value
}

// The consumer declares `fixtures__f_returns` for `f`; this function's own
// binding has the same name.
@onchain(steps: 100, semantics: e)
pub fn f_returns(value: Int) -> Int {
  value
}

//@ PROPERTY both for f f_returns
  "Both functions are called, so only the name collision can reject this."
  ∀ (x : Int),
    fixtures__f_returns (Data.I x) (Data.I x) ∧ fixtures__f_returns_returns (Data.I x) (Data.I x)
@//

validator companion {
  else(_ctx: Data) {
    fail
  }
}
