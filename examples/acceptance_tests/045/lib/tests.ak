fn fibonnaci(n) {
  if n == 0 {
    0
  } else if n == 1 {
    1
  } else {
    fibonnaci(n - 2) + fibonnaci(n - 1)
  }
}

test foo() {
  let right =
    fn() { fibonnaci(15) == 610 }
  let left = False
  left || right()
}
