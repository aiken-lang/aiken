fn must_be_signed(signatories) {
  when signatories is {
    [] -> {
      trace @"no signatories"
      False
    }
    [sig, ..] -> (sig == "#ffff")?
  }
}

fn must_say_hello(msg) {
  msg == "Hello, World!"
}

test foo() {
  must_be_signed(["#f000"])? && must_say_hello("Hello, World!")? || True
}
