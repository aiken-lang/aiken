fn label(str: String) -> Void {
  trace str
  Void
}

test foo() {
  label(@"Foo")
  True
}
