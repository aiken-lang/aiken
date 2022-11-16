pub type ScriptContext(purpose) {
  transaction: Transaction,
  purpose: purpose,
}

pub type Hash(a) =
  ByteArray

pub type ScriptPurpose {
  Mint(PolicyId)
  Spend(OutputReference)
  Withdrawal(StakeCredential)
  Certify(Certificate)
}

pub type BoundValue(value) {
  NegativeInfinity
  Finite(value)
  PositiveInfinity
}

pub type Bound(value) {
  value: BoundValue(value),
  is_inclusive: Bool,
}

pub type Interval(value) {
  lower_bound: Bound(value),
  upper_bound: Bound(value),
}

pub type Transaction {
  inputs: List(Input),
  reference_inputs: List(Input),
  outputs: List(Nil),
  fee: Value,
  mint: Value,
  certificates: List(Certificate),
  withdrawals: List(Pair(StakeCredential, Int)),
  validity_range: Interval(Int),
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

pub type StakeCredential =
  Nil

pub type VerificationKey =
  Nil

pub type PoolId =
  Hash(VerificationKey)

pub type Output =
  Nil

pub type AssetName =
  ByteArray

pub type Pair(a, b) =
  Nil

pub type Value =
  List(Pair(PolicyId, List(Pair(AssetName, Int))))

pub type Certificate {
  CredentialRegistration { delegator: StakeCredential }
  CredentialDeregistration { delegator: StakeCredential }
  CredentialDelegation { delegator: StakeCredential, delegatee: PoolId }
  PoolRegistration { pool_id: PoolId, vrf: Hash(VerificationKey) }
  PoolDeregistration { pool_id: PoolId, epoch: Int }
  Governance
  TreasuryMovement
}
