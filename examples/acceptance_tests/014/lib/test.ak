pub fn range(from: Int, to: Int) -> List<Int> {
  if from > to {
    []
  } else {
    [from, ..range(from + 1, to)]
  }
}

test range_1() {
  range(0, 2) == [0, 1, 2]
}

// NOTE:
// Somehow, the left-hand evaluates to: [#02, #01, #00, #32]
test range_2() {
  range(0 - 1, 2) == [0 - 1, 0, 1, 2]
}
