pub fn and_f(self: List<Bool>) -> Bool {
  when self is {
    [] -> True
    [x, ..xs] -> x && and_f(xs)
  }
}

test and_f_1() {
  and_f([True, True])
}

pub fn or_f(self: List<Bool>) -> Bool {
  when self is {
    [] -> False
    [x, ..xs] -> x || or_f(xs)
  }
}

test or_f_1() {
  or_f([True, True])
}
