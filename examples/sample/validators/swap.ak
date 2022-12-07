use sample

pub fn spend(datum: sample.Datum, rdmr: sample.Redeemer, _ctx: Nil) -> Bool {
  let x = #(datum, rdmr, #[244])

  datum.random == rdmr.signer
}
