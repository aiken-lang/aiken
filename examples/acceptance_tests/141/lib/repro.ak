fn helper_fail_bool() -> Bool {
  fail @"constant helper bool"
}

const broken_fail: Bool = helper_fail_bool()

test use_fail_constant() fail {
  broken_fail
}
