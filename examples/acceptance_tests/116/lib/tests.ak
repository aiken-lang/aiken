use aiken/builtin.{unconstr_fields, unconstr_index}

test bar() {
  let x = Some("bar")

  expect [bar] = x |> builtin.unconstr_fields

  and {
    unconstr_index(x) == 0,
    builtin.un_b_data(bar) == "bar",
  }
}

test baz() {
  let x = Some("baz")

  expect [baz] = x |> unconstr_fields

  and {
    builtin.unconstr_index(x) == 0,
    builtin.un_b_data(baz) == "baz",
  }
}
