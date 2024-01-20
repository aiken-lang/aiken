pub fn foldr(self: List<a>, with: fn(a, b) -> b, zero: b) -> b {
  when self is {
    [] -> zero
    [x, ..xs] -> with(x, foldr(xs, with, zero))
  }
}

pub fn concat(left: List<a>, right: List<a>) -> List<a> {
  foldr(left, fn(x, xs) { [x, ..xs] }, right)
}

pub fn flat_map(self: List<a>, with: fn(a) -> List<b>) -> List<b> {
  foldr(self, fn(x, xs) { concat(with(x), xs) }, [])
}

test flat_map_1() {
  flat_map([], fn(a) { [a] }) == []
}

test flat_map_2() {
  flat_map([1, 2, 3], fn(a) { [a, a] }) == [1, 1, 2, 2, 3, 3]
}
