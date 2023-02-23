use aiken/builtin

pub fn slice(bytes: ByteArray, start: Int, end: Int) -> ByteArray {
  builtin.slice_bytearray(start, end, bytes)
}

pub fn length(bytes: ByteArray) -> Int {
  builtin.length_of_bytearray(bytes)
}

pub fn drop(bytes: ByteArray, n: Int) -> ByteArray {
  slice(bytes, n, length(bytes) - n)
}

test drop_1() {
  let x = #"01020304050607"
  drop(x, 2) == #"0304050607"
}
