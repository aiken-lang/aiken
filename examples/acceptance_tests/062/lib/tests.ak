fn bar(n) {
  n == 42
}

test foo() {
  if bar(42) {
    True
  } else {
    False
  }
}
