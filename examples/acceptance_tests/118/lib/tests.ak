use aiken/builtin.{write_bits}

test bar() {
  let x =
    if True {
      [0, 1, 2, 3]
    } else {
      [0, 1]
    }

  write_bits(#"f0", x, True) == #"ff"
}

test baz() {
  let x = [0, 1, 2, 3]
  write_bits(#"f0", x, True) == #"ff"
}

test bur() {
  let x =
    if True {
      [0, 1, 2, 3]
    } else {
      [0, 1]
    }

  if False {
    fn(_a, _b, _c) { #"" }
  } else {
    write_bits
  }(
    #"f0",
    x,
    True,
  ) == #"ff"
}
