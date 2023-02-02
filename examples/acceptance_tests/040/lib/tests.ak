use aiken/builtin

pub type Door {
  angle: Int,
  locked: Bool,
}

pub type Car {
  Honda { remote_connect: ByteArray, owner: ByteArray, wheels: Int }
  Ford {
    remote_connect: ByteArray,
    owner: ByteArray,
    wheels: Int,
    truck_bed_limit: Int,
    car_doors: List<Door>,
  }
}

test update_owner1() {
  let initial_car =
    builtin.constr_data(
      1,
      [
        builtin.b_data(#""),
        builtin.b_data(#""),
        builtin.i_data(4),
        builtin.i_data(10000),
        builtin.list_data([]),
      ],
    )
  assert Ford { owner, wheels, truck_bed_limit, .. }: Car = initial_car
  owner == #"" && wheels == 4 && truck_bed_limit == 10000
}
