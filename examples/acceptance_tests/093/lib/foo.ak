pub fn wow(a: Void) -> Int {
  when Some(a) is {
    Some(Void) -> 42
    None -> 0
  }
}

test wow_1() {
  wow(Void) == 42
}
