test foo() {
  let xs = [[1, 2], [4, 5]]
  when xs is {
    [[_, _], [_, _]] -> True
    _ -> False
  }
}
