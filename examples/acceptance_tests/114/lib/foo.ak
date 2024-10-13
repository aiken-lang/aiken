use aiken/builtin

pub type Decoder<a> =
  fn(ByteArray) -> Option<(a, ByteArray)>

pub fn data() -> Decoder<Data> {
  fn(bytes) {
    when int()(bytes) is {
      Some((n, "")) -> Some((builtin.i_data(n), ""))
      _ ->
        when list(data())(bytes) is {
          Some((xs, "")) -> Some((builtin.list_data(xs), ""))
          _ -> None
        }
    }
  }
}

/// Dummy implementation: always decode two elements
pub fn list(decode_one: Decoder<a>) -> Decoder<List<a>> {
  fn(bytes) {
    when decode_one(bytes) is {
      Some((a0, bytes)) ->
        when decode_one(bytes) is {
          Some((a1, bytes)) -> Some(([a0, a1], bytes))
          None -> None
        }
      None -> None
    }
  }
}

/// Dummy implementation: consume the next byte and yield 42.
pub fn int() -> Decoder<Int> {
  fn(bytes) {
    Some(
      (
        42,
        builtin.slice_bytearray(
          1,
          builtin.length_of_bytearray(bytes) - 1,
          bytes,
        ),
      ),
    )
  }
}

test decode_data() {
  let d: Data = [42, 42]
  data()(#"0000") == Some((d, ""))
}
