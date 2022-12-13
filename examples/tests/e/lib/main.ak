use aiken/builtin.{head_list}

pub fn head(xs: List<a>) -> Option<a> {
  when xs is {
    [] -> None
    _ -> Some(head_list(xs))
  }
}

test head_1() {
  head([1, 2, 3]) == Some(1)
}

test head_2() {
  head([]) == None
}
