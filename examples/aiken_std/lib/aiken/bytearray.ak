use aiken/builtin

pub fn slice(bytes: ByteArray, start: Int, end: Int) -> ByteArray {
  builtin.sliceByteArray(start, end, bytes)
}

pub fn length(bytes: ByteArray) -> Int {
  builtin.lengthOfByteArray(bytes)
}

pub fn concat(left front: ByteArray, right back: ByteArray) -> ByteArray {
  builtin.appendByteArray(front, back)
}
