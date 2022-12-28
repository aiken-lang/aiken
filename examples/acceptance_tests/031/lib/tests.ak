pub fn clamp(n: Int, min: Int, max: Int) -> Int {
  if n < min {
    min
  } else if n > max {
    max
  } else {
    n
  }
}

test clamp_1() {
  clamp(14, min: 0, max: 10) == 10
}
