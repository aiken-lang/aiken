use sample

pub fn spend(datum: sample.Datum, rdmr: sample.Redeemer, _ctx: Nil) -> Bool {
  datum.random == rdmr.signer
}
