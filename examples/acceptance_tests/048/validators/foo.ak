fn when_tuple(a: (Int, Int)) -> Int {
  when a is {
    (a, b) -> a
  }
}

pub fn spend(a, b, c) -> Bool {
  when_tuple((4, 1)) == 4
}
