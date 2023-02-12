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

test expect_ford1() {
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
  expect Ford { owner, wheels, truck_bed_limit, .. }: Car = initial_car
  owner == #"" && wheels == 4 && truck_bed_limit == 10000
}


test expect_ford2() {
  let initial_car = Ford {remote_connect: #"", owner: #[34,34,34,34,34],  wheels: 6, truck_bed_limit: 15000, car_doors: []}
  expect Ford { owner, wheels, remote_connect, .. } = initial_car
  owner == #[34,34,34,34,34] && wheels == 6 && remote_connect == #""
}




test expect_list1() {
  let initial_car = [5,6,7]
  expect [a,b,c] = initial_car
  a == 5 && b == 6 && c == 7 
}



test expect_list2() {
  let initial_car = [5,6,7]
  expect [a, ..d] = initial_car
  a == 5 && d == [6, 7]
}
