test sort_by_1() {
  let xs =
    [[4, 3, 2, 1], [2, 3, 4, 5]]
  when xs is {
    [[], ys] -> False
    [xs, []] -> False
    [[x, ..xs2], [y, ..ys2]] -> True
    _ -> False
  }
}
