@list
type Dino {
  food: Int,
  weight: Int,
  name: ByteArray,
}

type Wow {
  @tag(2)
  Het { first: Dino, second: (Int, ByteArray) }
  Toro(Int)
}

test foo() {
  let x = Dino(1, 2, "t-rex")

  let y: Data = x

  expect (a, b, c): (Int, Int, ByteArray) = y
  expect z: Dino = y
  expect Dino { food, weight, name } = y
  let xx = Dino { ..z, name: "stegosaurus" }

  let yy = x |> as_data

  and {
    x.food == 1,
    a == 1,
    b == 2,
    c == "t-rex",
    x == z,
    food == 1,
    weight == 2,
    xx.food == z.food,
    xx.name == "stegosaurus",
    xx.weight == z.weight,
    yy == y,
  }
}

test bar() {
  let x = Dino(1, 2, "t-rex")
  let y = Het(x, (4, "no"))

  expect Het { first: Dino(1, b, "t-rex"), second: (4, "no") } = y |> as_data

  expect Het { first: Dino(1, c, "t-rex"), second: (4, "no") } = y

  when y is {
    Toro(1) -> False
    Toro(2) -> False
    Toro(_) -> fail @"No way"
    Het { first: Dino(1, 1, "t-rex"), second: (4, "no") } -> False
    Het { first: Dino(1, 2, "t-rex"), second: (5, "no") } -> False
    Het { first: Dino(1, 2, "t-rex"), second } -> and {
        b == 2,
        c == 2,
        second == (4, "no"),
      }
    Het { .. } -> fail @"Impossible"
  }
}
