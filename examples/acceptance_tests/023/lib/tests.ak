pub opaque type AssocList<key, value> {
  inner: List<(key, value)>,
}

pub fn new() -> AssocList<key, value> {
  AssocList { inner: [] }
}

pub fn to_list(m: AssocList<key, value>) -> List<(key, value)> {
  m.inner
}

pub fn insert(
  in m: AssocList<key, value>,
  key k: key,
  value v: value,
) -> AssocList<key, value> {
  AssocList { inner: do_insert(m.inner, k, v) }
}

fn do_insert(elems: List<(key, value)>, k: key, v: value) -> List<(key, value)> {
  when elems is {
    [] ->
      [(k, v)]
    [(k2, v2), ..rest] ->
      if k == k2 {
        [(k, v), ..rest]
      } else {
        [(k2, v2), ..do_insert(rest, k, v)]
      }
  }
}

fn fixture_1() {
  new()
    |> insert("foo", 42)
    |> insert("bar", 14)
}

test to_list_2() {
  to_list(fixture_1()) == [("foo", 42), ("bar", 14)]
}
