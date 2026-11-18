// Module `fixtures.extra`: the file name's dot is part of the module path.
// Blueprint titles keep it (`fixtures.extra.vesting.spend`); the emitted
// target id, which is also the Lean binding, is `fixtures_extra__vesting`.
@onchain(steps: 2500, semantics: e)
validator vesting(minimum: Int) {
  spend(_datum: Option<Data>, _redeemer: Data, _ref: Data, _tx: Data) {
    minimum > 0
  }

  else(_ctx: Data) {
    fail
  }
}

//@ PROPERTY dotted_minimum for vesting.spend
  "A nonpositive minimum rejects spending from a dotted module."
  ∀ (minimum : Int) (ctx : ScriptContext),
    minimum ≤ 0 → isUnsuccessful (fixtures_extra__vesting minimum (toLedgerData ctx))
@//
