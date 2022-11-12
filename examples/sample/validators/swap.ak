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

pub fn twice(f: fn(Int) -> Int, x: Int) -> Int {
  f(f(x))
}

pub fn add_one(x: Int) -> Int {
  x + 1
}

pub fn add_two(x: Int) -> Int {
  twice(add_one, x)
}

pub fn spend(
  datum: sample.Datum,
  rdmr: Redeemer,
  ctx: spend.ScriptContext,
) -> Bool {
 let x = rdmr.signer 
 let z = datum.sc.signer.hash
 let y = datum.sc
 let a = datum.fin

 a > 0 && z == x 

}
