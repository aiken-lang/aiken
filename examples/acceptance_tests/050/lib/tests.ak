use aiken/builtin

pub type Thing {
  wow: Int,
}

test let_1() {
  let x: Data = 1

  x == builtin.i_data(1)
}

test let_2() {
  let x: Data = 1

  expect y: Int = x

  y == 1
}

test assert_1() {
  expect thing: Thing = builtin.constr_data(0, [builtin.i_data(1)])

  thing.wow == 1
}

fn cast_to_thing(x: Data) -> Thing {
  expect x: Thing = x

  x
}

test assert_2() {
  let thing = Thing { wow: 1 }

  let still_thing = cast_to_thing(thing)

  still_thing.wow == 1
}

test tuple_1() {
  let thing = (#"aa", #"bb", #"cc")
  thing.1st == #"aa"
}

test pair_1() {
  let thing = (#"aa", #"bb")
  thing.1st == #"aa"
}
// should not typecheck
// test unlift_data_without_assert_1() {
//   let thing: Thing = builtin.constr_data(0, [builtin.i_data(1)])
//   thing.wow == 1
// }

// should not typecheck
// fn bad_cast(x: Thing) -> Int {
//   x.wow
// }

// test unlift_data_without_assert_2() {
//   let thing = builtin.constr_data(0, [builtin.i_data(1)])
//   bad_cast(thing) == 1
// }
