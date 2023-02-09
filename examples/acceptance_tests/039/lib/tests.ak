pub type Door {
  locked: Bool,
  hinge_angle: Int,
}

pub type Car {
  owner: ByteArray,
  wheels: Int,
  door: Door,
  vin: ByteArray,
}

pub fn update_owner(new_owner: ByteArray, car: Car) {
  Car { ..car, owner: new_owner }
}

pub fn update_vin(new_vin: ByteArray, car: Car) {
  Car { ..car, vin: new_vin }
}

pub fn update_door_angle(new_hinge_angle: Int, car: Car) {
  Car { ..car, door: Door { ..car.door, hinge_angle: new_hinge_angle } }
}

pub fn update_door_locked_and_wheels(
  new_locked: Bool,
  new_wheels: Int,
  car: Car,
) {
  Car {
    ..car,
    door: Door { ..car.door, locked: new_locked },
    wheels: new_wheels,
  }
}

test update_owner1() {
  let initial_car =
    Car {
      owner: #"",
      wheels: 4,
      vin: #"01010101010101",
      door: Door { locked: False, hinge_angle: 45 },
    }
  let final_car =
    Car {
      owner: #"f4f4f4f4",
      wheels: 4,
      vin: #"01010101010101",
      door: Door { locked: False, hinge_angle: 45 },
    }
  update_owner(#"f4f4f4f4", initial_car) == final_car
}

test update_vin1() {
  let initial_car =
    Car {
      owner: #"",
      wheels: 4,
      vin: #"01010101010101",
      door: Door { locked: False, hinge_angle: 45 },
    }
  let final_car =
    Car {
      owner: #"",
      wheels: 4,
      vin: #"020202020202020202",
      door: Door { locked: False, hinge_angle: 45 },
    }
  update_vin(#"020202020202020202", initial_car) == final_car
}

test update_door_angle1() {
  let initial_car =
    Car {
      owner: #"",
      wheels: 4,
      vin: #"01010101010101",
      door: Door { locked: False, hinge_angle: 45 },
    }
  let final_car =
    Car {
      owner: #"",
      wheels: 4,
      vin: #"01010101010101",
      door: Door { locked: False, hinge_angle: 90 },
    }
  update_door_angle(90, initial_car) == final_car
}

test update_door_locked_and_wheels1() {
  let initial_car =
    Car {
      owner: #"",
      wheels: 4,
      vin: #"01010101010101",
      door: Door { locked: False, hinge_angle: 45 },
    }
  let final_car =
    Car {
      owner: #"",
      wheels: 5,
      vin: #"01010101010101",
      door: Door { locked: True, hinge_angle: 45 },
    }
  update_door_locked_and_wheels(True, 5, initial_car) == final_car
}
