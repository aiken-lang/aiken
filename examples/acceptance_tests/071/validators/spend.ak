use aiken/collection/list
use aiken/crypto.{Blake2b_224, Hash}
use aiken/interval.{Finite}
use aiken/option
use cardano/assets.{
  AssetName, PolicyId, Value, add, flatten, from_asset, negate, quantity_of,
}
use cardano/address.{Address, Script, VerificationKey}
use cardano/transaction.{
  Datum, InlineDatum, Input, Output, OutputReference, Transaction, TransactionId,
  ValidityRange,
}

// Datum/Redeemer pool
pub type PoolDatum {
  currency_symbol: CurrencySymbol,
  balance: Int,
  lent_out: Int,
}

pub type PoolRedeemer {
  action: PoolRedeemerType,
}

pub type PoolRedeemerType {
  PoolWithdraw(Int)
  PoolDeposit(PoolDepositRedeemer)
  PoolBorrow(PoolBorrowRedeemer)
}

pub type PoolDepositRedeemer {
  input_cs: CurrencySymbol,
  input_amount: Int,
}

pub type PoolBorrowRedeemer {
  input_cs: CurrencySymbol,
  input_amount: Int,
}

pub type CurrencySymbol {
  policy_id: PolicyId,
  asset_name: AssetName,
}

pub fn get_output(transaction: Transaction, address: Address) -> Option<Output> {
  list.find(transaction.outputs, fn(output) { output.address == address })
}

pub fn get_input(transaction: Transaction, address: Address) -> Option<Input> {
  list.find(transaction.inputs, fn(input) { input.output.address == address })
}

pub fn scripthash_address(scripthash: ByteArray) {
  Address { payment_credential: Script(scripthash), stake_credential: None }
}

pub fn validate_pool_deposit(
  transaction: Transaction,
  output_reference: OutputReference,
  datum: PoolDatum,
  redeemer: PoolDepositRedeemer,
) -> Bool {
  let validator_address = scripthash_address(#"ff")

  expect Some(pool_output) = get_output(transaction, validator_address)
  expect Some(pool_input) = get_input(transaction, validator_address)

  True
}

pub fn validate_pool_borrow(
  transaction: Transaction,
  output_reference: OutputReference,
  datum: PoolDatum,
  redeemer: PoolBorrowRedeemer,
) -> Bool {
  let validator_address = scripthash_address(#"ff")

  expect Some(pool_output) = get_output(transaction, validator_address)
  expect Some(pool_input) = get_input(transaction, validator_address)
  True
}

validator foo {
  spend(
    datum: Option<PoolDatum>,
    redeemer: PoolRedeemer,
    output_ref: OutputReference,
    transaction: Transaction,
  ) {
    expect Some(datum) = datum
    when redeemer.action is {
      PoolWithdraw(_) -> True
      PoolDeposit(pool_deposit_redeemer) ->
        validate_pool_deposit(
          transaction,
          output_ref,
          datum,
          pool_deposit_redeemer,
        )
      PoolBorrow(pool_borrow_redeemer) ->
        validate_pool_borrow(
          transaction,
          output_ref,
          datum,
          pool_borrow_redeemer,
        )
    }
  }

  else(_) {
    fail
  }
}
