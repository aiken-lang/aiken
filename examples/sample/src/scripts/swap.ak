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
  if datum.something == "Aiken" {
    True
  } else if datum.something == "Wow" {
    True
  } else {
    False
  }
}
