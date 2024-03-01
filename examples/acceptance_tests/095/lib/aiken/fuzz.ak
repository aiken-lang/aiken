use aiken/builtin

const max_int: Int = 255

pub type PRNG {
  Seeded { seed: Int, choices: List<Int> }
  Replayed { choices: List<Int> }
}

pub type Fuzzer<a> =
  fn(PRNG) -> Option<(PRNG, a)>

// Primitives

pub fn any_int() -> Fuzzer<Int> {
  fn(prng: PRNG) -> Option<(PRNG, Int)> {
    when prng is {
      Seeded { seed, choices } -> {
        let digest =
          seed
            |> builtin.integer_to_bytearray(True, 32, _)
            |> builtin.blake2b_256()

        let choice =
          digest
            |> builtin.index_bytearray(0)

        let new_seed =
          digest
            |> builtin.slice_bytearray(1, 4, _)
            |> builtin.bytearray_to_integer(True, _)

        Some((Seeded { seed: new_seed, choices: [choice, ..choices] }, choice))
      }

      Replayed { choices } ->
        when choices is {
          [] -> None
          [head, ..tail] ->
            if head >= 0 && head <= max_int {
              Some((Replayed { choices: tail }, head))
            } else {
              None
            }
        }
    }
  }
}

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

pub fn map(fuzz_a: Fuzzer<a>, f: fn(a) -> b) -> Fuzzer<b> {
  fn(s0) {
    when fuzz_a(s0) is {
      Some((s1, a)) -> Some((s1, f(a)))
      None -> None
    }
  }
}

pub fn map2(fuzz_a: Fuzzer<a>, fuzz_b: Fuzzer<b>, f: fn(a, b) -> c) -> Fuzzer<c> {
  fn(s0) {
    when fuzz_a(s0) is {
      Some((s1, a)) ->
        when fuzz_b(s1) is {
          Some((s2, b)) -> Some((s2, f(a, b)))
          None -> None
        }
      None -> None
    }
  }
}

pub fn map4(
  fuzz_a: Fuzzer<a>,
  fuzz_b: Fuzzer<b>,
  fuzz_c: Fuzzer<c>,
  fuzz_d: Fuzzer<d>,
  f: fn(a, b, c, d) -> result,
) -> Fuzzer<result> {
  fn(s0) {
    when fuzz_a(s0) is {
      Some((s1, a)) ->
        when fuzz_b(s1) is {
          Some((s2, b)) ->
            when fuzz_c(s2) is {
              Some((s3, c)) ->
                when fuzz_d(s3) is {
                  Some((s4, d)) -> Some((s4, f(a, b, c, d)))
                  None -> None
                }
              None -> None
            }
          None -> None
        }

      None -> None
    }
  }
}

// Builders

fn any_bool() -> Fuzzer<Bool> {
  any_int() |> map(fn(n) { n <= 127 })
}

fn any_list(fuzz_a: Fuzzer<a>) -> Fuzzer<List<a>> {
  any_bool()
    |> and_then(
         fn(continue) {
           if continue {
             map2(fuzz_a, any_list(fuzz_a), fn(head, tail) { [head, ..tail] })
           } else {
             constant([])
           }
         },
       )
}
