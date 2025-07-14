pub type Bad {
  A
  B
  C
}

pub fn fine(b0: Bad, b1: Bad) -> Bad {
  when (b0, b1) is {
    (A, A) -> A
    (B, _) -> B
    (C, _) -> C
    (_, B) -> B
    (_, C) -> C
  }
}

pub fn crash(b0: Bad, b1: Bad) -> Bad {
  when (b0, b1) is {
    (A, A) -> A
    (B, _) -> B
    (_, B) -> B
    (C, _) -> C
    (_, C) -> C
  }
}

test test_fine() {
  let b0 = A
  let b1 = B
  fine(b0, b1) == b1
}

test test_crash() {
  let b0 = A
  let b1 = B
  crash(b0, b1) == b1
}
