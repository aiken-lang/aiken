pub fn when_tuple(a: (Int, Int)) -> Int {
  when a is {
    (a, b) -> a
  }
}

test when_tuple_1() {
  when_tuple((4, 1)) == 4
}
