@onchain(steps: 100, semantics: e)
pub fn target(value: Int) -> Int {
  value
}

//@ PROPERTY missing_fragment_helper for target
  "The referenced fragment must belong to the property closure."
  ∀ (value : Int), helpers.valid value
@//
