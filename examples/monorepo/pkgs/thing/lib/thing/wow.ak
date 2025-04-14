use aiken/fuzz
use aiken/primitive/bytearray
use aiken/primitive/int
 
fn sample_bytearray(size: Int) -> Fuzzer<ByteArray> {
  fuzz.bytearray_between(size * 128, size * 128)
}
 
bench bytearray_length(bytes: ByteArray via sample_bytearray) {
  bytearray.length(bytes)
}
 
// Note, you can also omit the `: ByteArray` type annotation
bench bytearray_to_integer(bytes via sample_bytearray) {
  int.from_bytearray_big_endian(bytes)
}
