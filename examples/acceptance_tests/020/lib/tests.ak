pub fn map(opt: Option<a>, f: fn(a) -> result) -> Option<result> {
  when opt is {
    None -> None
    Some(a) -> Some(f(a))
  }
}

test map_2() {
  map(Some(14), fn(n) { n + 1 }) == Some(15)
}
