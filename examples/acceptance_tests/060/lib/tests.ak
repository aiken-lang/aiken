type Foo {
  foo: Int,
}

const foo_constant = Foo { foo: 42 }

test foo() {
  foo_constant == Foo { foo: 42 }
}

const tuple_constant = (1, 2, 3)

test bar() {
  tuple_constant == (1, 2, 3)
}
