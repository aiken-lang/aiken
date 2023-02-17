const int_constant = 42

test int() {
  int_constant == 42
}

const bytearray_constant = #"abcd"

test bytearray() {
  bytearray_constant == #"abcd"
}

const string_constant = "FOO"

test string() {
  string_constant == "FOO"
}
