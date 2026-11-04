@onchain(steps: 100, semantics: e)
pub fn target(value: Int) -> Int {
  value
}

//@ PROPERTY unterminated_block for target
  "The annotation must have a closing delimiter."
  True
