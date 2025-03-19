pub type Foo {
  i: Int,
  b: Bool,
}

const give_i: fn(Int) -> Foo = Foo { i: _, b: True }

const give_b: fn(Bool) -> Foo = Foo(1337, _)

fn foo_i(i: Int) -> fn(Bool) -> Foo {
  Foo { i: i, b: _bool }
}

fn foo_b(b: Bool) -> fn(Int) -> Foo {
  Foo(_, b)
}

test test_1() {
  let bar = foo_i(14)
  and {
    foo_i(42)(True) == Foo(42, True),
    bar(False) == Foo { i: 14, b: False },
    give_i(1337) == Foo { i: 1337, b: True },
  }
}

test test_2() {
  let bar = foo_b(False)
  and {
    foo_b(True)(42) == Foo(42, True),
    bar(14) == Foo { i: 14, b: False },
    give_b(False) == Foo { i: 1337, b: False },
  }
}
