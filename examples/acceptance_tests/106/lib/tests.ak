type Foo {
  foo: Int,
}

type Bar {
  Buzz
  Bazz
}

test soft_casting_1() {
  let data: Data = Foo { foo: 42 }
  if data is Foo {
    True
  } else {
    False
  }
}

test soft_casting_2() {
  let data: Data = Foo { foo: 42 }
  if data is Foo { .. }: Foo {
    True
  } else {
    False
  }
}

test soft_casting_3() {
  let data: Data = Buzz
  if data is Foo {
    False
  } else {
    True
  }
}

test soft_casting_4() {
  let data: Data = Buzz
  if data is Foo { .. }: Foo {
    False
  } else {
    True
  }
}

test soft_casting_5() {
  let data: Data = Bazz
  if data is Foo {
    False
  } else {
    True
  }
}

test soft_casting_6() {
  let data: Data = Bazz
  if data is Foo { .. }: Foo {
    False
  } else {
    True
  }
}

type A {
  A(Bool)
}

type B {
  B(Int)
}

test soft_casting_7() {
  let data: Data = A(True)
  if data is B(_): B {
    False
  } else {
    True
  }
}
