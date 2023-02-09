use aiken/builtin

pub fn is_empty(bytes: ByteArray) -> Bool {
  builtin.length_of_bytearray(bytes) == 0
}

test is_empty_1() {
  is_empty(#"") == True
}

test is_empty_1_alt() {
  is_empty(#"")
}

test is_empty_2() {
  is_empty(#"01") == False
}

test is_empty_2_alt() {
  !is_empty(#"01")
}
