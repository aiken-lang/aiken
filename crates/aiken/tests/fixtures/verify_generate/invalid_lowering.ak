// This typechecks, but a function-only bundle violates the pinned blueprint dialect.
@onchain(steps: 100, semantics: e)
pub fn f() {
  True
}

//@ PROPERTY function_only for f "A valid function without a validator." True @//
