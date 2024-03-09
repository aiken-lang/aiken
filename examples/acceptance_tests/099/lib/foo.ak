fn countdown_1() {
  fn(x) {
    if x > 4 {
      countdown_3()
    } else {
      x - countdown_2()
    }
  }
}

fn countdown_2() {
  countdown_1()(5)
}

fn countdown_3() {
  3
}

fn countdown_4() {
  fn(x) {
    if x > 4 {
      countdown_6()
    } else {
      x - countdown_5(x + 1)
    }
  }
}

fn countdown_5(x: Int) {
  countdown_4()(x)
}

fn countdown_6() {
  3
}

test cycle_zero_arg_1() {
  countdown_1()(3) == 0
}

test cycle_zero_arg_2() {
  countdown_4()(3) == 2
}
