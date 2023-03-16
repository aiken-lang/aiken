use aiken/builtin

fn concat(left: String, right: String) -> String {
  builtin.append_bytearray(
    builtin.encode_utf8(left),
    builtin.encode_utf8(right),
  )
    |> builtin.decode_utf8
}

fn is_negative(i: Int) -> Bool {
  if i < 0 {
    trace @"is negative"
    True
  } else {
    trace concat(@"is", concat(@" ", @"non-negative"))
    False
  }
}

test trace_1() {
  is_negative(-14) && !is_negative(42)
}
