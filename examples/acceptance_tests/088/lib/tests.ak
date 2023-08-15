use aiken/math/rational.{Rational}
use aiken/option

pub fn div_by_int_orig(rat: Rational, token_2_amount: Int) -> Option<Int> {
  rational.div(
    rat,
    token_2_amount
      |> rational.from_int,
  )
    |> option.map(rational.truncate)
}

pub fn div_by_int_fixed(rat: Rational, token_2_amount: Int) -> Option<Int> {
  when
    rational.div(
      rat,
      token_2_amount
        |> rational.from_int,
    )
  is {
    Some(rat) -> Some(rational.truncate(rat))
    None -> None
  }
}

test sale_failing_test() {
  div_by_int_orig(100 |> rational.from_int, 100) == Some(1)
}

test sale_fixed_test() {
  div_by_int_fixed(100 |> rational.from_int, 100) == Some(1)
}
