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
  let y = [datum.fin, 2, 3]
  let z = [1, ..y]
  when z is {
    [] -> False
    [a] -> a == 1
    [a, b] -> b == 2
    [a, b, c] -> a > 1
    [a, b, c, ..d] -> b > 1
    _other -> True
  }
}
