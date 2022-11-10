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
  let x = rdmr.amount
  let z = Buy(rdmr.signer, 55) 


  True
}
