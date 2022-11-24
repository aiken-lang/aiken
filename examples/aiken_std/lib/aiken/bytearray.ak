use aiken/builtin

pub fn slice(bytes: ByteArray, start: Int, end: Int) -> ByteArray {
  builtin.slice_bytearray(start, end, bytes)
}

pub fn length(bytes: ByteArray) -> Int {
  builtin.length_of_bytearray(bytes)
}

pub fn is_empty(bytes: ByteArray) -> Bool {
  length(bytes) == 0
}

pub fn concat(left front: ByteArray, right back: ByteArray) -> ByteArray {
  builtin.append_bytearray(front, back)
}

pub fn prepend(rest: ByteArray, byte: Int) -> ByteArray {
  builtin.cons_bytearray(byte, rest)
}
