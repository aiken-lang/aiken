pub fn any(items: List<a>, predicate: fn(a) -> Bool) -> Bool {
  when items is {
    [] -> False
    [item, ..rest] -> predicate(item) || any(rest, predicate)
  }
}
