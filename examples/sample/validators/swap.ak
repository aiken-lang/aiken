use sample
use sample/mint
use sample/spend
use aiken/builtin

const something = 5

pub type Redeemer {
  signer: ByteArray,
  amount: Int,
}

pub type Reen {
  Buy1 { signer: ByteArray, amount: Int }
  Sell1
}

pub fn twice(f: fn(Int) -> Int, initial: Int) -> Int {
  f(f(initial))
}

pub fn add_one(value: Int) -> Int {
  value + 1
}

pub fn add_two(x: Int) -> Int {
  twice(add_one, x)
}

pub fn final_check(z: Int) {
  z < 4
}

pub fn incrementor(counter: Int, target: Int) -> Int {
  if counter == target {
    target
  } else {
    incrementor(counter + 1, target)
  }
}

pub fn who(a: ByteArray) -> ByteArray {
  builtin.append_bytearray(a, #[12, 255])
}

pub type Datum {
  Buy
  Sell
}

pub fn spend(datum: Datum, _rdmr: Nil, _ctx: Nil) -> Bool {
  let x = [[[2, 9], [7]], [[4]], [[6]]]

  let [[[a, g], d], b, ..] = x

  True
}
