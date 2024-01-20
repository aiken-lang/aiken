pub fn choice(self: List<Option<a>>) -> Option<a> {
  when self is {
    [] -> None
    [Some(x), ..] -> Some(x)
    [None, ..others] -> choice(others)
  }
}

test choice_1() {
  choice([Some(14), Some(42)]) == Some(14)
}

test choice_2() {
  choice([]) == None
}

test choice_3() {
  choice([None]) == None
}

test choice_4() {
  choice([None, Some(42)]) == Some(42)
}
