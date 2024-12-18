fn simple_sampler(): Sampler<Int> {
  fn(n: Int) {
    fn(prng: PRNG) {
      n
    }
  }
}

fn run_sampler(sampler: Sampler<Int>, complexity: Int): Fuzzer<Int> {
  sampler(complexity)
}

test sampler_ok_basic() {
  let sampler = simple_sampler()

  run_sampler(sampler, 10)(Seeded { seed: "", choices: "" }) == 10
  run_sampler(sampler, 1)(Seeded { seed: "", choices: "" }) == 1
  run_sampler(sampler, 0)(Seeded { seed: "", choices: "" }) == 0
  run_sampler(sampler, 42)(Seeded { seed: "", choices: "" }) == 42
}
