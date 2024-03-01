use aiken/dict.{Dict}
use aiken/fuzz
use aiken/int

pub type Season {
  Winter
  Spring
  Summer
  Fall
}

fn compare_season(a: Season, b: Season) -> Ordering {
  let season_to_int =
    fn(season) {
      when season is {
        Winter -> 0
        Spring -> 1
        Summer -> 2
        Fall -> 3
      }
    }

  int.compare(season_to_int(a), season_to_int(b))
}

fn any_year() -> Fuzzer<Dict<Season, Int>> {
  fuzz.map4(
    fuzz.any_int(),
    fuzz.any_int(),
    fuzz.any_int(),
    fuzz.any_int(),
    fn(a, b, c, d) {
      dict.new()
        |> dict.insert(Winter, a, compare_season)
        |> dict.insert(Spring, b, compare_season)
        |> dict.insert(Summer, c, compare_season)
        |> dict.insert(Fall, d, compare_season)
    },
  )
}

test prop_always_cold_in_winter(year via any_year()) {
  when dict.get(year, Winter) is {
    Some(temperature) -> temperature <= 10
    _ -> fail @"failed to get?"
  }
}
