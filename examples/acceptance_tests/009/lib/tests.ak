use aiken/builtin.{length_of_bytearray}

pub fn is_empty(bytes: ByteArray) -> Bool {
  length_of_bytearray(bytes) == 0
}

test is_empty_1() {
  is_empty(#"") == True
}
