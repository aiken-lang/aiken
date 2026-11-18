@onchain(steps: 2500, semantics: e)
validator vesting(minimum: Int) {
  spend(_datum: Option<Data>, _redeemer: Data, _ref: Data, _tx: Data) {
    minimum > 0
  }

  else(_ctx: Data) {
    fail
  }
}

//@ PROPERTY positive_minimum for vesting.spend
  "A nonpositive minimum rejects spending."
  ∀ (minimum : Int) (ctx : ScriptContext),
    minimum ≤ 0 → isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx))
@//

//@ PROPERTY fallback_rejects for vesting.else
  "All non-spending invocations fail."
  ∀ (minimum : Int) (ctx : ScriptContext),
    isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx))
@//
