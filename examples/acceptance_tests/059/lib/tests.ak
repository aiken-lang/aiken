test foo() {
  let xs =
    [[1, 2], [4, 5]]
  when xs is {
    [[_, _], [_, _]] -> True
    _ -> False
  }
}

test sort_by_1() {
  let xs =
    [[4, 3], [2, 3]]
  let g =
    when xs is {
      [[x, xs2], [y, ys2]] -> True
      _ -> False
    }

  g == True
}
