pub type Foo {
  a: Int,
}

pub type Bar {
  Bazz(Int)
  Buzz(Int)
}

test if_soft_cast() {
  let d: Data = Foo { a: 1 }

  if d is Foo {
    d.a == 1
  } else {
    False
  }
}

test if_soft_cast_2() {
  let d: Data = Bazz(1)

  if d is Foo {
    d.a == 1
  } else if d is Bazz(y): Bar {
    y == 1
  } else {
    False
  }
}

test if_soft_cast_3() {
  let d: Data = Bazz(1)
  let x: Data = Buzz(2)

  if d is Foo {
    d.a == 1
  } else if d is Bazz(y): Bar {
    y == 1
  } else if x is Buzz(y): Bar {
    y == 2
  } else {
    False
  }
}
