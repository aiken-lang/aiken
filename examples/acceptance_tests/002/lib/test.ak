pub fn repeat(x: a, n: Int) -> List<a> {
  if n <= 0 {
    []
  } else {
    [x, ..repeat(x, n - 1)]
  }
}

test repeat_1() {
  repeat("aiken", 2) == ["aiken", "aiken"]
}
