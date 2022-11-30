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
  when counter is {
    target -> target
    _ -> incrementor(counter+1, target)
  }
}

pub fn who(a: ByteArray) -> ByteArray {
  builtin.append_bytearray(a, #[12, 255])
}

pub type Datum {
  Offer { price: Int, asset_class: ByteArray, thing: Int }
  Sell
  Hold(Int)
}

pub fn spend(datum: Datum, _rdmr: Nil, _ctx: Nil) -> Bool {
  when datum is {
    Offer { price, thing: t, .. } -> add_one(price) > 0
    Hold(less) -> incrementor(0, 8) == less
    Sell -> add_two(1) > 1
  }
}
