use aiken/builtin

const generator_g1: G1Element =
  #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

test bls12_381_g1_multi_scalar_mul() {
  let scalars: List<Int> = []

  let points: List<G1Element> = []

  let multi = builtin.bls12_381_g1_multi_scalar_mul(scalars, points)

  let identity = builtin.bls12_381_g1_scalar_mul(0, generator_g1)

  builtin.bls12_381_g1_equal(multi, identity)
}
