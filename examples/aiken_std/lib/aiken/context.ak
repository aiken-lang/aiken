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

pub type Redeemer =
  Data

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
  outputs: List(Output),
  fee: Value,
  mint: Value,
  certificates: List(Certificate),
  withdrawals: List(Pair(StakeCredential, Int)),
  validity_range: Interval(Int),
  extra_signatories: List(PublicKeyHash),
  redeemers: List(Pair(ScriptPurpose, Redeemer)),
  datums: List(Pair(Hash(Data), Data)),
  id: TransactionId,
}

pub type TransactionId {
  hash: Hash(Transaction),
}

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

pub type StakeCredential {
  StakeHash(Credential)
  StakePointer(Int, Int, Int)
}

pub type Credential {
  PublicKeyCredential(PublicKeyHash)
  ScriptCredential(ScriptHash)
}

pub type VerificationKey =
  Nil

pub type ScriptHash =
  ByteArray

pub type PublicKeyHash =
  Hash(VerificationKey)

pub type PoolId =
  Hash(VerificationKey)

pub type Output {
  address: Address,
  value: Value,
  datum: DatumOption,
  reference_script: Option(ScriptHash),
}

pub type Address {
  payment_credential: Credential,
  stake_credential: Option(StakeCredential),
}

pub type DatumOption {
  NoDatum
  DatumHash(Hash(Data))
  Datum(Data)
}

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
