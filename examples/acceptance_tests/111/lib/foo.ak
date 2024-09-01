type Point =
  (ByteArray, Int)

type NestedTuples {
  points: (Point, Point),
}

test boom() {
  let original = NestedTuples { points: (("", 14), ("foo", 42)) }

  let data: Data = original

  expect recovered: NestedTuples = data

  original == recovered
}
