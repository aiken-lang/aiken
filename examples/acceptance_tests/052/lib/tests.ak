type TransactionId {
  inner: ByteArray,
}

test pattern_match_let() {
  let x = TransactionId { inner: #"0000" }
  let TransactionId(y) = x
  y == #"0000"
}

test pattern_match_when() {
  let x = TransactionId { inner: #"0000" }
  let y =
    when x is {
      TransactionId(y) -> y
    }
  y == #"0000"
}
