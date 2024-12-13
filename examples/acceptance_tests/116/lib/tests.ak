use aiken/builtin.{constr_index}

test baz() {
  let x = Some("bar")

  expect [bar] = x |> builtin.constr_fields

  and {
    constr_index(x) == 0,
    builtin.un_b_data(bar) == "bar",
  }
}
