use aiken/collection/list.{count as my_count} as native_list

test thing_1() {
  let x =
    [1, 2, 3, 4, 5, 5]

  my_count(x, fn(item) { item > 0 }) == 6
}

test thing_2() {
  let x =
    [1, 2, 3, 4, 5, 5]

  native_list.count(x, fn(item) { item > 0 }) == 6
}

test thing_3() {
  let x =
    [1, 2, 3, 4, 5, 5]

  let my_countx = my_count

  my_countx(x, fn(item) { item > 0 }) == 6
}

test thing_4() {
  let x =
    [1, 2, 3, 4, 5, 5]

  let my_countx = native_list.count

  my_countx(x, fn(item) { item > 0 }) == 6
}
