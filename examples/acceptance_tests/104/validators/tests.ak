use aiken/fuzz

type Foo {
  a0: Int,
  a1: Bool,
}

fn foo_1(Foo { a0, .. }) -> Int {
  a0 + 1
}

fn foo_2(Foo { a0, a1 } as foo) -> Int {
  if a1 {
    a0 + 1
  } else {
    foo.a0 - 1
  }
}

validator(Foo { a0, .. }: Foo) {
  fn foo_3(_data, _redeemer) {
    a0 == 1
  }
}

test example_1() {
  foo_1(Foo { a0: 1, a1: False }) == 2
}

test example_2() {
  foo_2(Foo { a0: 1, a1: False }) == 0
}

test example_3() {
  foo_3(Foo { a0: 1, a1: False }, "", "")
}

test example_4() {
  let foo_4 = fn(Foo { a1, .. }) { a1 }
  foo_4(Foo { a0: 1, a1: True })
}

test example_5((a, b) via fuzz.both(fuzz.int(), fuzz.int())) {
  a + b  == b + a
}
