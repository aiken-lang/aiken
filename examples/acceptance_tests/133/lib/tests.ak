use aiken/builtin

const generator_g2: G2Element =
  #<Bls12_381, G2>"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

test bls12_381_g2_multi_scalar_mul() {
  let scalars: List<Int> = []

  let points: List<G2Element> = []

  let multi = builtin.bls12_381_g2_multi_scalar_mul(scalars, points)

  let identity = builtin.bls12_381_g2_scalar_mul(0, generator_g2)

  builtin.bls12_381_g2_equal(multi, identity)
}
