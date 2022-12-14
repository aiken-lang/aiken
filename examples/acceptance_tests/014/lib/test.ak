// NOTE:
// Somehow, the left-hand evaluates to: [#00, #20, #21]
// whereas the right-hand evaluates to: [#21, #20, #00]
//
test foo() {
  [0 - 2, 0 - 1, 0] == [-2, -1, 0]
}
