use aiken/builtin

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

test filter_1() {
  filter([1, 2, 3, 4, 5, 6], fn(x) { builtin.mod_integer(x, 2) == 0 }) == [
    2, 4, 6,
  ]
}
