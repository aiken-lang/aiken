@onchain(steps: 2500, semantics: e)
validator vesting(minimum: Int) {
  spend(_datum: Option<Data>, _redeemer: Data, _ref: Data, _tx: Data) {
    minimum > 0
  }

  else(_ctx: Data) {
    fail
  }
}

//@ PROPERTY applied_spend for vesting.spend[00]
  "The deployed zero-minimum validator rejects spending."
  ∀ (ctx : ScriptContext),
    isUnsuccessful (fixtures__vesting (toLedgerData ctx))
@//

//@ PROPERTY applied_fallback for vesting.else[01]
  "The deployed positive-minimum validator rejects non-spending invocations."
  ∀ (ctx : ScriptContext),
    isUnsuccessful (fixtures__vesting (toLedgerData ctx))
@//

//@ PROPERTY universal_minimum for vesting.spend
  "A nonpositive minimum rejects spending for every deployment."
  ∀ (minimum : Int) (ctx : ScriptContext),
    minimum ≤ 0 → isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx))
@//
