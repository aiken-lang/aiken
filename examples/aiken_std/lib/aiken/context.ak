pub type ScriptContext(purpose) {
  transaction: Transaction,
  purpose: purpose,
}

pub type Transaction {
  inputs: List(Nil),
  outputs: List(Nil),
  fee: Nil,
  mint: Nil,
  certificates: List(Nil),
  withdrawals: List(Nil),
  validity_range: Nil,
  extra_signatories: Nil,
  datums: List(Nil),
  id: ByteArray,
}
