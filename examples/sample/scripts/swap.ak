use sample/syntax.{append}

pub type Datum {
  something: String,
}

pub type Redeemer {
  Buy
  Sell
}

pub fn validate(datum: Datum, rdmr: Redeemer, ctx: ScriptContext) -> Bool {
  when rdmr is {
    Buy -> True
    Sell -> datum.something == "Aiken"
  }
}