use sample/mint
use sample/spend

pub type Datum {
  something: String,
}

pub type Redeemer {
  Buy { id: Int }
  Sell(Int)
}

pub fn spend(datum: Datum, rdmr: Redeemer, ctx: spend.ScriptContext) -> Bool {
  when rdmr is {
    Buy -> True
    Sell -> datum.something == "Aiken"
  }
}
