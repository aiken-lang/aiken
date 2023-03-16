pub fn map(xs: List<a>, f: fn(a) -> result) -> List<result> {
  when xs is {
    [] ->
      []
    [x, ..rest] ->
      [f(x), ..map(rest, f)]
  }
}

test map_1() {
  map([], fn(n) { n + 1 }) == []
}
