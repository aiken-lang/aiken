test foo_1() {
  let a = False
  when a is {
    True -> False
    False -> True
  }
}

test foo_2() {
  let a = False
  let b =
    when a is {
      True -> 14
      False -> 42
    }
  b == 42
}
