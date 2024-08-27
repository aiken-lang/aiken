type Foo {
  Foo(Int, Never)
  Bar
}

test never_is_none() {
  let none: Option<Void> = None

  trace @"Never": Never
  trace @"None": none

  let data_never: Data = Never
  let data_none: Data = none

  data_never == data_none
}

test never_pattern_match() {
  when Foo(14, Never) is {
    Foo(x, Never) -> x == 14
    Bar -> False
  }
}

test never_assignment() {
  let Never = Never
  True
}

test never_wrong_cast() fail {
  let data: Data = Some(42)
  expect _: Never = data
}

test never_ok_cast() {
  let none: Option<Void> = None
  let data: Data = none
  expect _: Never = data
}
