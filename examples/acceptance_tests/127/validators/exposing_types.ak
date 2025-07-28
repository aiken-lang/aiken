@list
pub type Dino {
  food: Int,
  weight: Int,
  name: ByteArray,
}

pub type Wow {
  @tag(6908)
  Het { first: Dino, second: (Int, ByteArray) }
  Toro(Int)
  @tag(4)
  MaybeYes
  MaybeNo
}

validator happy(thing: Dino) {
  else(other_thing: Dino) {
    thing == other_thing
  }
}

validator other(thing: Wow) {
  else(other_thing: Wow) {
    thing == other_thing
  }
}
