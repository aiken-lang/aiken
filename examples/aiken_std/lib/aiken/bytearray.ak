use aiken/builtins

pub fn slice(bytes: ByteArray, start: Int, end: Int) -> ByteArray {
  builtins.sliceByteString(start, end, bytes)
}
