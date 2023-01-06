pub fn and(self: List<Bool>) -> Bool {
  when self is {
    [] -> True
    [x, ..xs] -> x && and(xs)
  }
}

test and_1() {
  and([True, True])
}

pub fn or(self: List<Bool>) -> Bool {
  when self is {
    [] -> False
    [x, ..xs] -> x || or(xs)
  }
}

test or_1() {
  or([True, True])
}
