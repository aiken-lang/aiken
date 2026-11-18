@onchain(steps: 100, semantics: e)
pub fn target(value: Int) -> Int {
  value + 1
}

//@ PROPERTY underscore_target for target "Underscore-separated module target." True @//
