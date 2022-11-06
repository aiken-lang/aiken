use sample
use sample/mint
use sample/spend

pub type Redeemer {
  Buy { id: Int }
  Sell(Int)
}


pub fn spend(datum: sample.Datum, rdmr: Redeemer, ctx: spend.ScriptContext) -> Bool {

  let y = 2
  let x = datum.sc.signer
  let a = datum.sc.signer.hash
  let b = datum.rdmr
  let c = 1
  c == 1
}
