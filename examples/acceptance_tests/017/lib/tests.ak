use aiken/builtin

pub fn slice(bytes: ByteArray, start: Int, end: Int) -> ByteArray {
  builtin.slice_bytearray(start, end, bytes)
}

pub fn take(bytes: ByteArray, n: Int) -> ByteArray {
  slice(bytes, 0, n)
}

test take_1() {
  take(#"010203", 2) == #"0102"
}
