pub fn length(xs: List<a>) -> Int {
  when xs is {
    [] -> 0
    [_, ..rest] -> 1 + length(rest)
  }
}

test length_1() {
  length([1, 2, 3]) == 3
}

test length_2() {
  length([]) == 0
}
