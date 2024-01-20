pub fn foldr(xs: List<a>, f: fn(a, b) -> b, zero: b) -> b {
  when xs is {
    [] -> zero
    [x, ..rest] -> f(x, foldr(rest, f, zero))
  }
}

pub fn concat(left: List<a>, right: List<a>) -> List<a> {
  foldr(left, fn(x, xs) { [x, ..xs] }, right)
}

pub fn flat_map(xs: List<a>, f: fn(a) -> List<b>) -> List<b> {
  when xs is {
    [] ->
      []
    [x, ..rest] -> concat(f(x), flat_map(rest, f))
  }
}

test flat_map_2() {
  flat_map([1, 2, 3], fn(a) { [a, a] }) == [1, 1, 2, 2, 3, 3]
}
