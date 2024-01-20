/// An opaque `Dict`. The type is opaque because the module maintains some
/// invariant, namely: there's only one occurrence of a given key in the dictionary.
pub opaque type Dict<key, value> {
  inner: List<(ByteArray, value)>,
}

pub fn toList(self: Dict<ByteArray, value>) {
  self.inner
}

/// Create a new map
pub fn new() -> Dict<key, value> {
  Dict { inner: [] }
}

pub fn insert(
  self: Dict<ByteArray, value>,
  key k: ByteArray,
  value v: value,
) -> Dict<key, value> {
  Dict {
    inner: do_insert_with(self.inner, k, v, fn(_, left, _right) { Some(left) }),
  }
}

pub fn union_with(
  left: Dict<key, value>,
  right: Dict<key, value>,
  with: fn(ByteArray, value, value) -> Option<value>,
) -> Dict<key, value> {
  Dict { inner: do_union_with(left.inner, right.inner, with) }
}

fn do_union_with(
  left: List<(ByteArray, value)>,
  right: List<(ByteArray, value)>,
  with: fn(ByteArray, value, value) -> Option<value>,
) -> List<(ByteArray, value)> {
  when left is {
    [] -> right
    [(k, v), ..rest] ->
      do_union_with(rest, do_insert_with(right, k, v, with), with)
  }
}

fn do_insert_with(
  self: List<(ByteArray, value)>,
  key k: ByteArray,
  value v: value,
  with: fn(ByteArray, value, value) -> Option<value>,
) -> List<(ByteArray, value)> {
  when self is {
    [] ->
      [(k, v)]
    [(k2, v2), ..rest] ->
      if k == k2 {
        when with(k, v, v2) is {
          Some(combined) ->
            [(k, combined), ..rest]
          None -> rest
        }
      } else {
        [(k2, v2), ..do_insert_with(rest, k, v, with)]
      }
  }
}
