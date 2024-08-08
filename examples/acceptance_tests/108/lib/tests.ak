use aiken/builtin

pub fn reduce(xs: List<a>, zero: b, do: fn(a, b) -> b) -> b {
  when xs is {
    [] -> zero
    [head, ..tail] -> do(head, reduce(tail, zero, do))
  }
}

pub fn inspect_1(self: Data, result: result) -> result {
  builtin.choose_data(
    self,
    fail,
    fail,
    reduce(builtin.un_list_data(self), result, fn(a, b) { inspect_1(a, b) }),
    {
      trace @"int"
      result
    },
    fail,
  )
}

test as_lambda() {
  inspect_1([14, 42], True)
}

pub fn inspect_2(self: Data, result: result) -> result {
  builtin.choose_data(
    self,
    fail,
    fail,
    reduce(builtin.un_list_data(self), result, inspect_2),
    {
      trace @"int"
      result
    },
    fail,
  )
}

test as_identifier() {
  inspect_2([14, 42], True)
}
