pub type ScriptContext(purpose) {
  tx_info: TxInfo,
  script_purpose: purpose,
}

pub type TxInfo {
  idk: Int,
}
