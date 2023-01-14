pub fn foo(_a: a) -> Bool {
  True
}

test test_1() {
  foo(1)
}

test test_2() {
  foo(1) && foo(2)
}
