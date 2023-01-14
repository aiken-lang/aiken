pub opaque type Map<key, value> {
  inner: List<(key, value)>,
}

pub fn new() {
  Map { inner: [] }
}

test new_1() {
  new() == Map { inner: [] }
}
