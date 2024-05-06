test panic_aiken() {
  and {
    True,
    should_fail(1),
  }
}

fn should_fail(num) {
  expect num == 0
  True
}
