@onchain(steps: 100, semantics: X)
pub fn target(value: Int) -> Int {
  value
}

//@ PROPERTY invalid_semantics for target "Only C and E semantics are supported." True @//
