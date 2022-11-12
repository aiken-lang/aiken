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

pub fn spend(
  datum: sample.Datum,
  rdmr: Redeemer,
  ctx: spend.ScriptContext,
) -> Bool {
  1 % 2 != 3 || 5 / 4 - 2 != 0


  True
}
