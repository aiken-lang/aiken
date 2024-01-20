pub opaque type AssocList<key, value> {
  inner: List<(key, value)>,
}

pub fn new() -> AssocList<key, value> {
  AssocList { inner: [] }
}

pub fn from_list(xs: List<(key, value)>) -> AssocList<key, value> {
  AssocList { inner: do_from_list(xs) }
}

fn do_from_list(xs: List<(key, value)>) -> List<(key, value)> {
  when xs is {
    [] ->
      []
    [(k, v), ..rest] -> do_insert(do_from_list(rest), k, v)
  }
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

pub fn union(
  left: AssocList<key, value>,
  right: AssocList<key, value>,
) -> AssocList<key, value> {
  AssocList { inner: do_union(left.inner, right.inner) }
}

fn do_union(
  left: List<(key, value)>,
  right: List<(key, value)>,
) -> List<(key, value)> {
  when left is {
    [] -> right
    [(k, v), ..rest] -> do_union(rest, do_insert(right, k, v))
  }
}

fn fixture_1() {
  new()
    |> insert("foo", 42)
    |> insert("bar", 14)
}

test union_1() {
  union(fixture_1(), new()) == fixture_1()
}
