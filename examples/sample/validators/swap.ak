use sample

pub fn spend(datum: sample.Datum, rdmr: sample.Redeemer, _ctx: Nil) -> Bool {
  let x = (datum, rdmr, #[244])
  let y = [(#[222], #[222]), (#[233], #[52])]
  let [z, f, ..g] = y
  let (a, b, _) = x
  z == (#[222], #[222])
}

test foo() {
  1 + 1 == 2
}
