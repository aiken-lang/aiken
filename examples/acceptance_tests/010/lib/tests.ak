pub fn map(opt: Option<a>, f: fn(a) -> b) -> Option<b> {
  when opt is {
    None -> None
    Some(a) -> Some(f(a))
  }
}

fn add_one(n: Int) -> Int {
  n + 1
}

test map_1() {
  map(None, add_one) == None
}
