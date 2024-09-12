pub fn constant(a: a) -> Fuzzer<a> {
  fn(s0) { Some((s0, a)) }
}

pub fn and_then(fuzz_a: Fuzzer<a>, f: fn(a) -> Fuzzer<b>) -> Fuzzer<b> {
  fn(s0) {
    when fuzz_a(s0) is {
      Some((s1, a)) -> f(a)(s1)
      None -> None
    }
  }
}

/// Construct a fuzzer that returns values not present in a given list.
fn nub(n: Int, fuzzer: Fuzzer<a>) -> fn(List<a>) -> Fuzzer<a> {
  fn(st) {
    if n <= 0 {
      fail @"gave up trying to find unique values: the fuzzer did not yield any *new* value after many tries!"
    } else {
      let a <- and_then(fuzzer)
      if False {
        nub(n - 1, fuzzer)(st)
      } else {
        constant(a)
      }
    }
  }
}

test thing() {
  let a = 1

  nub(2, constant(a))([])(Seeded { seed: #"01", choices: #"" }) == Some(
    (Seeded { seed: #"01", choices: #"" }, 1),
  )
}
