@onchain(steps: 2500, semantics: e)
validator vesting(minimum: Int) {
  spend(_datum: Option<Data>, _redeemer: Data, _ref: Data, _tx: Data) {
    minimum > 0
  }

  else(_ctx: Data) {
    fail
  }
}

// A valid property: generation still publishes nothing when another property is invalid.
//@ PROPERTY positive_minimum for vesting.spend
  "A nonpositive minimum rejects spending."
  ∀ (minimum : Int) (ctx : ScriptContext),
    minimum ≤ 0 → isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx))
@//

// spend is explicit on vesting, so vesting.else excludes it: no context satisfies both
// premises, and this impossible conclusion would otherwise hold vacuously.
//@ PROPERTY vacuous_impossible for vesting.spend vesting.else
  "An impossible conclusion should never verify."
  ∀ (minimum : Int) (ctx : ScriptContext),
    isUnsuccessful (fixtures__vesting minimum (toLedgerData ctx)) ∧ False
@//
