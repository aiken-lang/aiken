use remote.{target}

pub fn call_remote(value: Int) -> Int {
  target(value)
}

//@ PROPERTY wrong_module for target "Imported targets are not local scopes." True @//
