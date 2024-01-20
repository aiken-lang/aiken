pub fn foldr(xs: List<a>, f: fn(a, b) -> b, zero: b) -> b {
  when xs is {
    [] -> zero
    [x, ..rest] -> f(x, foldr(rest, f, zero))
  }
}

pub fn filter_map(xs: List<a>, f: fn(a) -> Option<b>) -> List<b> {
  foldr(
    xs,
    fn(x, ys) {
      when f(x) is {
        None -> ys
        Some(y) ->
          [y, ..ys]
      }
    },
    [],
  )
}

test filter_map_1() {
  filter_map([], fn(_) { Some(42) }) == []
}
