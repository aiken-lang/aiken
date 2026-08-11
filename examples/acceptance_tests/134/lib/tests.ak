use aiken/builtin

const generator_g1: G1Element =
  #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

const generator_g2: G2Element =
  #<Bls12_381, G2>"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

fn count_down(n: Int) -> List<Int> {
  if n <= 0 {
    []
  } else {
    [n, ..count_down(n - 1)]
  }
}

fn replicate_g1(point: G1Element, n: Int) -> List<G1Element> {
  if n <= 0 {
    []
  } else {
    [point, ..replicate_g1(point, n - 1)]
  }
}

fn replicate_g2(point: G2Element, n: Int) -> List<G2Element> {
  if n <= 0 {
    []
  } else {
    [point, ..replicate_g2(point, n - 1)]
  }
}

// 2*G + 3*G == 5*G, with constant argument lists
test bls12_381_g1_multi_scalar_mul_constant() {
  builtin.bls12_381_g1_multi_scalar_mul(
    [2, 3],
    [generator_g1, generator_g1],
  ) == builtin.bls12_381_g1_scalar_mul(5, generator_g1)
}

test bls12_381_g2_multi_scalar_mul_constant() {
  builtin.bls12_381_g2_multi_scalar_mul(
    [2, 3],
    [generator_g2, generator_g2],
  ) == builtin.bls12_381_g2_scalar_mul(5, generator_g2)
}

// 1*G + 2*G + 3*G + 4*G == 10*G, with argument lists built at runtime
test bls12_381_g1_multi_scalar_mul_dynamic() {
  builtin.bls12_381_g1_multi_scalar_mul(
    count_down(4),
    replicate_g1(generator_g1, 4),
  ) == builtin.bls12_381_g1_scalar_mul(10, generator_g1)
}

test bls12_381_g2_multi_scalar_mul_dynamic() {
  builtin.bls12_381_g2_multi_scalar_mul(
    count_down(4),
    replicate_g2(generator_g2, 4),
  ) == builtin.bls12_381_g2_scalar_mul(10, generator_g2)
}

// The builtin used as a value rather than being called directly
test bls12_381_g1_multi_scalar_mul_as_value() {
  let multi_scalar_mul =
    if count_down(1) == [1] {
      builtin.bls12_381_g1_multi_scalar_mul
    } else {
      fn(_scalars: List<Int>, _points: List<G1Element>) { generator_g1 }
    }

  multi_scalar_mul([2, 3], [generator_g1, generator_g1]) == builtin.bls12_381_g1_scalar_mul(
    5,
    generator_g1,
  )
}
