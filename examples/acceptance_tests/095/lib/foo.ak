fn sum_relative_collateral_values(
  in_values: List<((ByteArray, ByteArray), Int)>,
  out_values: List<((ByteArray, ByteArray), Int)>,
  _collateral_is_swappable: Bool,
  acc: (Int, Int),
) -> (Int, Int) {
  when (in_values, out_values) is {
    ([(_inAsset, _inVal), ..], [(_outAsset, _outVal), ..]) -> acc

    ([(_, _inVal), ..], []) -> acc

    ([], [(_, _outVal), ..]) -> acc

    ([], []) -> acc
  }
  // This was the only thing changed.
}

test thing() {
  (0, 0) == sum_relative_collateral_values(
    [(("", ""), 0)],
    [(("", ""), 0)],
    False,
    (0, 0),
  )
}
