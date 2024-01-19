test foo_1() fail {
  False
}

test foo_2() fail {
  expect 14 == 42
  True
}
