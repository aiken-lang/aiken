use aiken/builtins.{appendByteString}

pub type Bool {
  True
  False
}

pub fn append(a: ByteArray, b: ByteArray) -> ByteArray {
  appendByteString(a, b)
}