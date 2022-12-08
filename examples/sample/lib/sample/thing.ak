use aiken/builtin

test bar() {
  builtin.length_of_bytearray(#[2, 2, 3]) == 3
}
