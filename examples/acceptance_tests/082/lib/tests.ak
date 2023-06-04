use aiken/list
use aiken/transaction/value.{Value}

test foldl_value_test1() {
  let val1 = value.from_lovelace(1000000)
  let val2 = value.from_lovelace(2000000)
  let foo =
    fn(i: Value, acc: (Value, Int)) {
      let (v, int) = acc
      (value.add(i, v), int + 1)
    }
  list.foldl([val1, val2], foo, (value.zero(), 0)) == (
    value.from_lovelace(3000000),
    2,
  )
}
// test foldl_value_test2() {
//   let val1 = value.from_lovelace(1000000)
//   let val2 = value.from_lovelace(2000000)
//   let foo =
//     fn(i: Value, acc: (Value, Int)) {
//       let (v, int) = acc
//       (value.add(i, v), int + 1)
//     }
//   list.foldl([val1, val2], foo, (value.from_lovelace(0), 0)) == (
//     value.from_lovelace(3000000),
//     2,
//   )
// }
