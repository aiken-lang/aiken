use aiken/builtin

type MyData {
  Integer(Int)
  Bytes(ByteArray)
}

test foo() {
  inspect(42) == Integer(42) && inspect(#"ff") == Bytes(#"ff")
}

fn inspect(data: Data) -> MyData {
  expect result: MyData =
    builtin.choose_data(
      data,
      inspect_constr(data),
      inspect_map(data),
      inspect_list(data),
      inspect_integer(data),
      inspect_bytearray(data),
    )
  result
}

fn inspect_constr(_data: Data) -> Data {
  todo
}

fn inspect_map(_data: Data) -> Data {
  todo
}

fn inspect_list(_data: Data) -> Data {
  todo
}

fn inspect_integer(data: Data) -> Data {
  let result: Data =
    builtin.un_i_data(data)
      |> Integer
  result
}

fn inspect_bytearray(data: Data) -> Data {
  let result: Data =
    builtin.un_b_data(data)
      |> Bytes
  result
}
