type Foo {
  Bar { f0: Int, f1: Int }
  Baz { f0: Int, f1: Int }
}

fn predicate(self: Foo) -> Bool {
  when self is {
    Bar { f0, f1 } -> f0 == f1
    Baz{..} as baz -> baz.f0 < baz.f1
  }
}

test foo() {
  predicate(Baz { f0: 14, f1: 42 })? && predicate(Bar { f0: 42, f1: 42 })?
}
