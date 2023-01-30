pub type Door{
  angle: Int,
  locked: Bool
}


pub type Car {
  Honda { remote_connect: ByteArray, owner: ByteArray, wheels: Int }
  Ford {
    remote_connect: ByteArray,
    owner: ByteArray,
    wheels: Int,
    truck_bed_limit: Int,
    car_doors: List<Door>
  }
}

// test update_owner2_should_fail(){
//   let initial_car: Data = Ford{remote_connect: #[], owner: #[], wheels: 4, truck_bed_limit: 10000, car_doors: []}
//   assert Honda{ owner, ..}: Car = initial_car
//   owner == #[]
// }

test update_owner1() {
  let initial_car: Data =
    Ford { remote_connect: #[], owner: #[], wheels: 4, truck_bed_limit: 10000, car_doors: [] }
  assert Ford { owner, .. }: Car = initial_car
  owner == #[]
}
