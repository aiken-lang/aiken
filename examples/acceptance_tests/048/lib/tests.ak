test foo_1() {
  let a = False
  when a is {
    a if a -> False
    _ -> True
  }
}

test foo_2() {
  let point = (14, 42)
  when point is {
    (x, _) if x > 100 -> False
    (x, _) if x > 10 -> True
    _ -> False
  }
}

test foo_3() {
  let point = (14, 42)
  when point is {
    (x, y) if x == 14 && y <= 100 -> True
    _ -> False
  }
}

test foo_4() {
  let a = False
  let point = (14, 42)
  when point is {
    (x, y) if !a -> x + y == 56
    _ -> False
  }
}
