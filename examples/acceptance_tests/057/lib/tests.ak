pub fn alt(left: Option<a>, right: Option<a>) -> Option<a> {
  when (left, right) is {
    (Some(a), Some(_)) -> Some(a)
    (None, Some(a)) -> Some(a)
    (Some(a), None) -> Some(a)
    (None, None) -> None
  }
}

test foo() {
  Some(1) == alt(Some(1), Some(2))
}
