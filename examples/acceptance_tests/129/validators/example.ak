use aiken/builtin

fn publish(certificate: Foo, outputs: List<ByteArray>) {
  when certificate is {
    A { credential } | B { credential } | C { credential } -> {
      let expected = builtin.head_list(outputs)
      // The left operand here should NOT be curried by the compiler,
      // since it is actually depending on runtime values that may be
      // _error_
      expected == credential
    }
    D { .. } -> True
  }
}

type Foo {
  A { credential: ByteArray }
  B { credential: ByteArray }
  C { credential: ByteArray }
  D { credential: ByteArray }
}

test success() {
  expect publish(D { credential: "" }, [])
}
