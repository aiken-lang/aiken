test expect_positive() {
  let val = 5
  expect val > 0
  True
}

test expect_false() {
  let val = 5
  expect False = val < 0
  True
}

test expect_data_false() {
  let val: Data = 0 > 5
  expect False: Bool = val
  True
}

test expect_data_true() {
  let val: Data = 0 < 5
  expect True: Bool = val
  True
}
