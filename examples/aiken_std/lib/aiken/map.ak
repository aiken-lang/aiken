pub opaque type Map(key, value) {
  inner: List(#(key, value)),
}

/// Create a new map
pub fn new() {
  Map { inner: [] }
}

/// Get the inner list holding the map data
pub fn to_list(m: Map(key, value)) -> List(#(key, value)) {
  m.inner
}

/// Get a value in the map by a key
///
/// ```aiken
/// use aiken/map
///
/// let info = map.new() |> map.insert(key: "name", value: "Aiken")
/// 
/// asset Some(x) = map.get(in: info, by: "key")
/// ```
pub fn get(in m: Map(key, value), by k: key) -> Option(value) {
  do_get(m.inner, k)
}

fn do_get(elems: List(#(key, value)), k: key) -> Option(value) {
  when elems is {
    [] -> None
    [#(first, second), ..xs] ->
      if first == k {
        Some(second)
      } else {
        do_get(xs, k)
      }
  }
}

/// Insert a value in the map by a key
///
/// ```aiken
/// use aiken/map
///
/// map.new() |> map.insert(key: "name", value: "Aiken")
/// ```
pub fn insert(
  in m: Map(key, value),
  key k: key,
  value v: value,
) -> Map(key, value) {
  if contains(m, k) {
    m
  } else {
    Map { inner: [#(k, v), ..m.inner] }
  }
}

/// Check if a key exists in the map
///
/// ```aiken
/// use aiken/map
///
/// let info = map.new() |> map.insert(key: "name", value: "Aiken")
/// 
/// asset Some(x) = map.contains(in: info, key: "key")
/// ```
pub fn contains(in m: Map(key, value), key k: key) -> Bool {
  do_contains(m.inner, k)
}

fn do_contains(elems: List(#(key, value)), k: key) -> Bool {
  when elems is {
    [] -> False
    [#(first, _), ..xs] ->
      if first == k {
        True
      } else {
        do_contains(xs, k)
      }
  }
}
