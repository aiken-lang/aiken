type PRNG {
  seed: Int,
  size: Int,
}

fn any_int(prng: PRNG) {
  (prng, prng.seed)
}

test prop_test_foo(n via any_int) {
  n > 0
}
