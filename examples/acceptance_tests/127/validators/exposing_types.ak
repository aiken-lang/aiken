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
}

validator happy(thing: Dino){
  else(other_thing: Dino) -> Bool{
    thing == other_thing
  }
}


validator other(thing: Wow){
  else(other_thing: Wow) -> Bool{
    thing == other_thing
  }
}
