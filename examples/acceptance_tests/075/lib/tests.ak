pub opaque type Dict<key, value> {
  inner: List<Pair<key, value>>,
}

pub fn constant(a: a) -> Fuzzer<a> {
  fn(s0) { Some((s0, a)) }
}

pub fn map(fuzz_a: Fuzzer<a>, f: fn(a) -> b) -> Fuzzer<b> {
  fn(s0) {
    when fuzz_a(s0) is {
      Some((s1, a)) -> Some((s1, f(a)))
      None -> None
    }
  }
}

// NOTE: Inlining `do_list` fixes the problem. But the indirection here causes:
//
// --> Type mismatch expected 'pair data data' got 'data'
pub fn list(fuzzer: Fuzzer<a>) -> Fuzzer<List<a>> {
  do_list(fuzzer, [])
}

fn do_list(fuzzer, xs) -> Fuzzer<List<a>> {
  let x <- map(fuzzer)
  [x, ..xs]
}

pub fn dict() -> Fuzzer<Dict<Int, Bool>> {
  list(constant(Pair(1, True)))
    |> map(fn(inner) { Dict { inner } })
}

test prop_dict_between(_d via dict()) {
  True
}
