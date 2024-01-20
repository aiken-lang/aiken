test foo_1() {
  let a = False
  when a is {
    a if a -> False
    _ -> True
  }
}

test foo_2() {
  let point = (14, 42)
  when point is {
    (x, _) if x > 100 -> False
    (x, _) if x > 10 -> True
    _ -> False
  }
}

test foo_3() {
  let point = (14, 42)
  when point is {
    (x, y) if x == 14 && y <= 100 -> True
    _ -> False
  }
}

test foo_4() {
  let a = False
  let point = (14, 42)
  when point is {
    (x, y) if !a -> x + y == 56
    _ -> False
  }
}

type Seasons {
  Winter
  Spring
  Summer
  Fall
}

fn is_cold(season, hour) {
  when season is {
    Winter | Fall -> True
    _ if hour >= 18 -> True
    _ -> False
  }
}

test foo_5() {
  !is_cold(Spring, 15) && is_cold(Summer, 22)
}

fn when_tuple(a: (Int, Int)) -> Int {
  when a is {
    (a, _b) -> a
  }
}

test foo_6() {
  when_tuple((4, 1)) == 4
}
