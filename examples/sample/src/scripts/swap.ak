use sample
use sample/mint
use sample/spend

pub type Redeemer {
  Buy { id: Int }
  Sell(Int)
}


pub fn spend(datum: sample.Datum, rdmr: Redeemer, ctx: spend.ScriptContext) -> Bool {

  let x = 1
  let a = when datum is {
    sample.Datum2(first, rest) -> rest
    sample.None -> 0
  }
  let b = x
  b == 1

  

}
