use sample
use sample/mint
use sample/spend

pub type Redeemer {
  signer: ByteArray,
  amount: Int
}

pub type Reen {
  Buy{
    signer: ByteArray,
    amount: Int
  }
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

pub fn final_check(z: Int){
  z < 4
}

pub fn spend(
  datum: sample.Datum,
  rdmr: Redeemer,
  ctx: spend.ScriptContext,
) -> Bool {

 let a = datum.fin

 a 
 |> add_two 
 |> final_check
}
