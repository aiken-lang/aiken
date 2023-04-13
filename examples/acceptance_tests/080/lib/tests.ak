test foo1() {
  let bar: Data = Void

  let x = True

  expect Void: Void = bar

  // let Void = bar
  x
}

test foo2() {
  let bar = Void

  let x = True

  expect Void: Void = bar

  // let Void = bar
  x
}

test foo3() {
  let bar = Void

  let x = True

  let Void: Void = bar

  // let Void = bar
  x
}
