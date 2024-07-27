fn new_list() {
  []
}

test foo() {
  fn() { new_list }()() == []
}
