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
  Buy { signer: ByteArray, amount: Int }
  Sell
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
  if counter == target then {
    target
  } else {
    incrementor(counter + 1, target)
  }
}

pub fn who(a: ByteArray) -> ByteArray {
  builtin.append_bytearray(a, #[12, 255])
}

pub fn spend(
  datum: sample.Datum,
  rdmr: Redeemer,
  ctx: spend.ScriptContext,
) -> Bool {
  when [1, 2, 3] is {
    [a, b, ..] -> True
    [] -> False
  }
}
