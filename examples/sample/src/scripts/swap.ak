use sample
use sample/mint
use sample/spend

pub type Redeemer {
  Buy { id: Int }
  Sell(Int)
}


pub fn spend(datum: sample.Datum, rdmr: Redeemer, ctx: spend.ScriptContext) -> Bool {
  let a = True
  let b = sample.thing(a)
  let c = sample.thing(b)

  c
}
