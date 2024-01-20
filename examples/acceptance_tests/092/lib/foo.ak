test foo_1() {
  let a = {
    let b = 42
    b
  }

  a == 42
}

test foo_2() {
  expect Some(a) = {
    let b = 42
    Some(b)
  }

  a == 42
}

test foo_3() {
  let c = Some(42)

  let a = {
    expect Some(b) = c
    b
  }

  a == 42
}

test foo_4() {
  let a = {
    let b = 2
    let c = {
      let d = 14
      d * b
    }
    c + 14
  }

  a == 42
}
