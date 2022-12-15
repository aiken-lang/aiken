pub fn foldr(xs: List<a>, f: fn(a, b) -> b, zero: b) -> b {
  when xs is {
    [] -> zero
    [x, ..rest] -> f(x, foldr(rest, f, zero))
  }
}

pub fn prepend(x: a, xs: List<a>) -> List<a> {
  [x, ..xs]
}

pub fn concat(left: List<a>, right: List<a>) -> List<a> {
  foldr(left, prepend, right)
}

test concat_1() {
  concat([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]
}
