pub fn unzip(xs: List<(a, b)>) -> (List<a>, List<b>) {
  when xs is {
    [] -> ([], [])
    [(a, b), ..rest] -> {
      let (a_tail, b_tail) = unzip(rest)
      ([a, ..a_tail], [b, ..b_tail])
    }
  }
}

test unzip1() {
  let x =
    [(3, #"55"), (4, #"7799")]

  unzip(x) == ([3, 4], [#"55", #"7799"])
}
