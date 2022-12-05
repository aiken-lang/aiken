use sample
use sample/mint
use sample/spend
use aiken/builtin

const something = 5

pub type Redeemer {
  signer: ByteArray,
  amount: Int,
  other_thing: Redeemer,
}

pub type Reen {
  Buy1 { signer: ByteArray, amount: Int }
  Stuff(ByteArray, Int)
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
    counter
  } else if counter > target {
    counter - target
  } else {
    incrementor(counter + 1, target)
  }
}

pub type Datum {
  Offer { prices: List(Int), asset_class: ByteArray, other_thing: Datum }
  Sell
  Hold(Int)
}

pub fn spend(datum: Datum, _rdmr: Nil, _ctx: Nil) -> Bool {
  let amount = 1000
  let x = Buy1 { signer: #[4, 4, 255], amount }
  when x is {
    Buy1 { signer, .. } -> signer == #[3, 3, 255]
    Stuff(signer, _) -> signer == #[3, 3, 255]
    _ -> False
  }
}
