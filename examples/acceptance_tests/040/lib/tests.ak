


pub type Car{
  Honda { remote_connect: ByteArray, owner: ByteArray, wheels: Int}
  Ford { remote_connect: ByteArray, owner: ByteArray, wheels: Int, truck_bed_limit: Int }
}



test update_owner1(){
  let initial_car: Data = Ford{remote_connect: #[], owner: #[], wheels: 4, truck_bed_limit: 10000}
  assert Ford{ owner, ..} = initial_car
  owner == #[]
  
}
