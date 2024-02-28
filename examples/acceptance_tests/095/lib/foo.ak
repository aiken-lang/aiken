use aiken/builtin

const max_int: Int = 255

pub type PRNG {
  Seeded { seed: Int, choices: List<Int> }
  Replayed { choices: List<Int> }
}

type Fuzzer<a> =
  fn(PRNG) -> Option<(PRNG, a)>

// Primitives

fn any_int(prng: PRNG) -> Option<(PRNG, Int)> {
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

// Builders

fn any_bool() -> Fuzzer<Bool> {
  any_int |> map(fn(n) { n <= 127 })
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

fn any_season() -> Fuzzer<Season> {
  any_int
    |> and_then(
         fn(i) {
           let n = i % 3
           if n == 0 {
             any_bool() |> map(Winter)
           } else if n == 1 {
             constant(Spring(i))
           } else if n == 2 {
             constant(Summer)
           } else {
             constant(Fall)
           }
         },
       )
}

fn length(xs: List<a>) -> Int {
  when xs is {
    [] -> 0
    [_, ..tail] -> 1 + length(tail)
  }
}

fn filter(xs: List<a>, f: fn(a) -> Bool) -> List<a> {
  when xs is {
    [] ->
      []
    [head, ..tail] ->
      if f(head) {
        [head, ..filter(tail, f)]
      } else {
        filter(tail, f)
      }
  }
}

// Properties

pub type Season {
  Winter(Bool)
  Spring(Int)
  Summer
  Fall
}

// test prop_is_never_summer(xs via any_list(any_season())) {
//   filter(xs, fn(x) { x == Summer }) == []
// }

test prop_is_always_cold_in_winter(xs via any_list(any_season())) {
  is_always_cold_in_winter(xs)
}

test prop_is_always_cold_in_winter_2() {
  is_always_cold_in_winter([Winter(True)])
}

fn is_always_cold_in_winter(xs: List<Season>) -> Bool {
  when xs is {
    [Winter(cold), ..tail] -> cold && is_always_cold_in_winter(tail)
    _ -> True
  }
}
