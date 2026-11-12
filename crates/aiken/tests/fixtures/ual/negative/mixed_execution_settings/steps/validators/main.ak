@onchain(steps: 100, semantics: e)
pub fn first(value: Int) -> Int {
  value
}

@onchain(steps: 101, semantics: e)
pub fn second(value: Int) -> Int {
  value + 1
}

//@ PROPERTY mixed_execution_settings for first second
  "All scoped targets must use identical execution settings."
  True
@//
