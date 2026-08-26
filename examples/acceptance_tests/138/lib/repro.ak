use aiken/builtin

pub type MyDatum<a> {
  Constructor1(a)
  Constructor2
}

fn bad_data() -> Data {
  builtin.constr_data(0, [builtin.b_data(#"aa")])
}

fn helper(d: Data) -> Bool {
  expect dat: MyDatum<Int> = d

  when dat is {
    Constructor1(_) -> True
    _ -> False
  }
}

test plain_helper_typed_expect() fail {
  helper(bad_data())
}

test traced_helper_typed_expect() fail {
  trace @"user trace": helper(bad_data())
}
