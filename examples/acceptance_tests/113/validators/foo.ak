use aiken/builtin

pub const generator_g1: G1Element =
  #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

pub const generator_g2: G2Element =
  #<Bls12_381, G2>"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

validator here {
  mint(_rdr, _policy_id, _tx) {
    let g1 = builtin.bls12_381_g1_compress(generator_g1)

    let g2 = builtin.bls12_381_g2_compress(generator_g2)

    g1 != g2
  }

  else(_) {
    fail
  }
}
