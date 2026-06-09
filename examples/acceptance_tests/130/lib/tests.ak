use aiken/builtin.{index_array, length_of_array, list_to_array}

pub fn from_list(xs: List<a>) -> Array<a> {
  list_to_array(xs)
}

pub fn length(xs: Array<a>) -> Int {
  length_of_array(xs)
}

pub fn at(xs: Array<a>, ix: Int) -> Option<a> {
  if ix < 0 || ix >= length(xs) {
    None
  } else {
    Some(index_array(xs, ix))
  }
}

test from_list_length() {
  length(from_list([11, 22, 33])) == 3
}

test at_in_bounds() {
  at(from_list([11, 22, 33]), 1) == Some(22)
}

test at_out_of_bounds_hi() {
  at(from_list([11, 22, 33]), 3) == None
}

test at_out_of_bounds_lo() {
  at(from_list([11, 22, 33]), -1) == None
}
