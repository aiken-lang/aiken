type No {
  id: Int,
}

validator wow {
  spend(_datum: Option<Data<No>>, _redeemer: Data<No>, _oref, _ctx) {
    True
  }

  else(_ctx) {
    fail
  }
}

pub fn thing(_a, _cont: fn(a) -> Bool) {
  1
}
