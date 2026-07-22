use aiken/builtin

fn native_identity(value: Value) -> Value {
  value
}

fn same_value(left: Value, right: Value) -> Bool {
  builtin.value_contains(left, right) && builtin.value_contains(right, left)
}

test native_value_identity() {
  expect _ = native_identity(#<Value>[])
  True
}

test insert_and_lookup() {
  let value = builtin.insert_coin(#"aa", #"beef", 42, #<Value>[])
  and {
    builtin.lookup_coin(#"aa", #"beef", value) == 42,
    builtin.lookup_coin(#"bb", #"beef", value) == 0,
  }
}

test union_and_scale() {
  let left = #<Value>[(#"", [(#"", 5)])]
  let right = #<Value>[(#"", [(#"", 7)]), (#"aa", [(#"beef", 1)])]
  let scaled = builtin.scale_value(2, builtin.union_value(left, right))
  and {
    builtin.lookup_coin(#"", #"", scaled) == 24,
    builtin.lookup_coin(#"aa", #"beef", scaled) == 2,
  }
}

test contains_and_data_roundtrip() {
  let value = #<Value>[(#"", [(#"", 2)]), (#"aa", [(#"beef", 3)])]
  let smaller = #<Value>[(#"aa", [(#"beef", 1)])]
  and {
    builtin.value_contains(value, smaller),
    !builtin.value_contains(smaller, value),
    same_value(builtin.un_value_data(builtin.value_data(value)), value),
  }
}
