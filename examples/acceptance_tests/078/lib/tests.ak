type DayOfTheWeek {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday
  Sunday
}

fn is_work(day: DayOfTheWeek) {
  when day is {
    Tuesday | Wednesday | Thursday | Friday | Saturday -> True
    _ -> False
  }
}

test is_work_1() {
  is_work(Thursday)
}

test is_work_2() {
  !is_work(Monday)
}

fn is_happy_hour(day: DayOfTheWeek, current_time: Int) {
  when day is {
    Monday | Sunday -> True
    Tuesday | Wednesday | Thursday | Friday | Saturday -> current_time > 18
  }
}

test is_happy_hour_1() {
  is_happy_hour(Sunday, 14)
}

test is_happy_hour_2() {
  is_happy_hour(Friday, 22)
}
