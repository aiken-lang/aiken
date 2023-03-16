pub fn filter(xs: List<a>, f: fn(a) -> Bool) -> List<a> {
  when xs is {
    [] ->
      []
    [x, ..rest] ->
      if f(x) {
        [x, ..filter(rest, f)]
      } else {
        filter(rest, f)
      }
  }
}

pub fn unique(xs: List<a>) -> List<a> {
  when xs is {
    [] ->
      []
    [x, ..rest] ->
      [x, ..unique(filter(rest, fn(y) { y != x }))]
  }
}

test unique_1() {
  unique([]) == []
}

test unique_2() {
  let xs =
    [1, 2, 3, 1]
  unique(xs) == [1, 2, 3]
}
