pub fn unzip(xs: List<(a, b)>) -> (List<a>, List<b>) {
  when xs is {
    [] -> ([], [])
    [(a, b), ..rest] -> {
      let (a_tail, b_tail) = unzip(rest)
      ([a, ..a_tail], [b, ..b_tail])
    }
  }
}

test unzip_1() {
  unzip([(1, "a"), (2, "b")]) == ([1, 2], ["a", "b"])
}
