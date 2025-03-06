validator my_script(_param: ByteArray) {
  mint(_redeemer: Int, _policy_id: Data, _self: Data) {
    True
  }

  spend(_datum: Option<Int>, _redeemer: Bool, _utxo: Data, _self: Data) {
    True
  }

  else(_) {
    fail
  }
}
