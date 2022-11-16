pub type ScriptContext(purpose) {
  transaction: Transaction,
  purpose: purpose,
}

pub type Hash(a) =
  ByteArray

pub type ScriptPurpose {
  Mint(PolicyId)
  Spend(OutputReference)
  Withdrawal(Nil)
}

pub type Transaction {
  inputs: List(Input),
  reference_inputs: List(Input),
  outputs: List(Nil),
  fee: Value,
  mint: Value,
  certificates: List(Nil),
  withdrawals: List(Nil),
  validity_range: Nil,
  extra_signatories: Nil,
  redeemers: List(Nil),
  datums: List(Pair(Hash(Data), Data)),
  id: TransactionId,
}

pub type TransactionId =
  Hash(Transaction)

pub type Input {
  output_reference: OutputReference,
  output: Output,
}

pub type OutputReference {
  transction_id: TransactionId,
  output_index: Int,
}

pub type PolicyId =
  ByteArray

pub type Output =
  Nil

pub type AssetName =
  ByteArray

pub type Pair(a, b) =
  Nil

pub type Value =
  List(Pair(PolicyId, List(Pair(AssetName, Int))))
