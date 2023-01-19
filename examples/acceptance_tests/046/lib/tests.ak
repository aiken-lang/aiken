test sort_by_1() {
  let xs = [[4, 3, 2, 1], [2, 3, 4, 5]]
  when xs is {
    [[], ys] -> True
    [xs, []] -> True
    [[x, ..xs2], [y, ..ys2]] -> True
    _ -> True
  }
}
