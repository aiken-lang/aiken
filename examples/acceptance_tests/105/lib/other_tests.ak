type Foo {
  msg: Int,
}

type Bar {
  Buzz
  Bazz
}

test thing() {
  let data: Data = Buzz
  if data is Foo {
    False
  } else {
    True
  }
}

type A<x> {
  NoA
  SomeA(Void, x)
}

type B {
  something: Void,
}

test err_example() {
  let r: Data = SomeA(Void, B(Void))
  expect x: A<B> = r

  when x is {
    NoA -> False
    SomeA(_, B(something)) -> something == Void
  }
}
