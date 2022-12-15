pub fn map(opt: Option<a>, f: fn(a) -> result) -> Option<result> {
  when opt is {
    None -> None
    Some(a) -> Some(f(a))
  }
}

test map_1() {
  map(None, fn(_) { 14 }) == None
}

test map_2() {
  map(None, fn(_) { Nil }) == None
}
