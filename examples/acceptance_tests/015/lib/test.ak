pub opaque type Map<key, value> {
  inner: List<#(key, value)>,
}

pub fn new() {
  Map { inner: [] }
}

// Seems to be an issue with the parser. The TypedExpr body is excluding the Equal binop.
test new_1() {
  new() == Map { inner: [] }
}
