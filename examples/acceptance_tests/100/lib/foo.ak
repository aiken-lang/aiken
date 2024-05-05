pub fn list(fuzzer: Option<a>) -> Option<List<a>> {
  inner(fuzzer, [])
}

fn inner(fuzzer, xs) -> Option<List<b>> {
  when fuzzer is {
    None -> Some(xs)
    Some(x) -> Some([x, ..xs])
  }
}

test foo() {
  list(None) == Some([])
}
