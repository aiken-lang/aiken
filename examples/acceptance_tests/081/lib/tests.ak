use other.{FFF, GGG}

test thing1() {
  let x = other.FFF

  x == FFF
}

test thing2() {
  let x = other.GGG(2)

  x == GGG(2)
}
