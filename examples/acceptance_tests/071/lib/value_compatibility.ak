use aiken/builtin

fn native_identity(value: Value) -> Value {
  value
}

fn same_value(left: Value, right: Value) -> Bool {
  builtin.contains_value(left, right) && builtin.contains_value(right, left)
}

test native_value_identity() {
  expect _ = native_identity({})
  True
}

test insert_and_lookup() {
  let value = builtin.insert_value(#"aa", #"beef", 42, {})
  and {
    builtin.lookup_value(#"aa", #"beef", value) == 42,
    builtin.lookup_value(#"bb", #"beef", value) == 0,
  }
}

test union_and_scale() {
  let left = {
    #"00000000000000000000000000000000000000000000000000000000": {
      "": 5,
    },
  }
  let right = {
    #"00000000000000000000000000000000000000000000000000000000": {
      #"": 7,
    },
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa": {
      #"beef": 1,
    },
  }
  let scaled = builtin.scale_value(2, builtin.union_value(left, right))
  and {
    builtin.lookup_value(
      #"00000000000000000000000000000000000000000000000000000000",
      #"",
      scaled,
    ) == 24,
    builtin.lookup_value(
      #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      #"beef",
      scaled,
    ) == 2,
  }
}

test contains_and_data_roundtrip() {
  let value = {
    #"00000000000000000000000000000000000000000000000000000000": {
      "": 2,
    },
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa": {
      #"beef": 3,
    },
  }
  let smaller = {
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa": {
      #"beef": 1,
    },
  }
  and {
    builtin.contains_value(value, smaller),
    !builtin.contains_value(smaller, value),
    same_value(builtin.un_value_data(builtin.value_data(value)), value),
  }
}
