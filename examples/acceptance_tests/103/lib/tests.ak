fn do_diagnostic(self: Data) -> ByteArray {
  let builder = 2

  from_int(builder)
}

fn from_int(i: Int) -> ByteArray {
  "dddd"
}

test other() {
  do_diagnostic(1) != "thing"
}
