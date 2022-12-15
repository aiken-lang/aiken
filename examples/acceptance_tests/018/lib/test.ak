pub fn or_else(opt: Option<a>, default: a) -> a {
  when opt is {
    None -> default
    Some(a) -> a
  }
}

test or_else_2() {
  or_else(Some(42), 14) == 42
}
