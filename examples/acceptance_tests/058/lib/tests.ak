fn whatever(_xs) {
  True
}

test foo() {
  let xs =
    [1, 2, 3]
  when xs is {
    [x] -> x == 1
    _ -> whatever(xs)
  }
}

test bar() {
  let xs =
    [1, 2, 3]
  when xs is {
    [x] -> x == 1
    ys -> whatever(ys)
  }
}
