use sample
use sample/mint
use sample/spend

pub type Redeemer {
  Buy { id: Int }
  Sell(Int)
}


pub fn spend(datum: sample.Datum, rdmr: Redeemer, ctx: spend.ScriptContext) -> Bool {

  let x = "x"
  let a = "y"
  let b = sample.eqString(x, a)
  let c = 1
  let y = 1
  let d = sample.eqInt(y, c)

  if b {
    c == 1
  } else{
    d
  }

}
