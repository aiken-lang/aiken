use aiken/builtin

const max_int: Int = 255

type PRNG {
  Seeded { seed: Int, choices: List<Int> }
  Replayed { choices: List<Int> }
}

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

test prop_foo_1(n via any_int) {
  n >= 0 && n <= 255
}

test prop_foo_2(n via any_int) fail {
  n < 100
}
