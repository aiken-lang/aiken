fn is_negative(i: Int) -> Bool {
  if i < 0 {
    trace("is negative")

  True
  } else {
    trace("is non-negative")

  False
  }
}

test trace_1() {
  is_negative(-14) && !is_negative(42)
}
