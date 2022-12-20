pub fn abs(a: Int) -> Int {
  if a < 0 {
    -a
  } else {
    a
  }
}

test abs_1() {
  abs(-14) == 14
}
