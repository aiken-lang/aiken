pub opaque type Dict<a, b> {
  inner: List<(a, b)>,
}

pub opaque type Value {
  inner: Dict<ByteArray, Dict<ByteArray, Int>>,
}

fn to_dict(v: Value) {
  v.inner
}

fn to_list(d: Dict<a, b>) {
  d.inner
}

test opaque_destructure() {
  let x = Value { inner: Dict { inner: [(#"", Dict { inner: [("ab", 3)] })] } }

  expect [(policy_a, Dict{inner: [(asset_name_a, quantity_a), ..]}), ..] =
    x |> to_dict() |> to_list()

  quantity_a > 2
}

test opaque_destructure2() {
  let x = Value { inner: Dict { inner: [(#"", Dict { inner: [("ab", 3)] })] } }

  when x |> to_dict() |> to_list() is {
    [] -> fail
    [(policy_a, Dict{inner: []}), ..] -> fail
    [(policy_a, Dict{inner: [(asset_name_a, quantity_a), ..]}), ..] ->
      quantity_a > 2
  }
}
