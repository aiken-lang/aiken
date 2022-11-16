use sample
use sample/mint
use sample/spend

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

pub fn spend(
  datum: sample.Datum,
  rdmr: Redeemer,
  ctx: spend.ScriptContext,
) -> Bool {
  let x = datum.rdmr
  when x is {
    sample.Buy(tipo1, fin) -> fin > 0
    sample.Sell(twice, find: fin) -> fin > 0
    sample.Hold(some) -> some > 0
  }
}
