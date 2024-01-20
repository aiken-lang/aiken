use aiken/hash.{Blake2b_224, Hash}
use aiken/interval.{Finite}
use aiken/list
use aiken/option
use aiken/transaction.{
  Datum, InlineDatum, Input, Mint, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId, ValidityRange,
}
use aiken/transaction/credential.{
  Address, Script, ScriptCredential, VerificationKey, VerificationKeyCredential,
}
use aiken/transaction/value.{
  AssetName, PolicyId, Value, add, flatten, from_asset, negate, quantity_of,
}

// Datum/Redeemer pool
pub type PoolDatum {
  currency_symbol: CurrencySymbol,
  balance: Int,
  lent_out: Int,
}

type PoolRedeemer {
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

pub fn get_output(ctx: ScriptContext, address: Address) -> Option<Output> {
  list.find(ctx.transaction.outputs, fn(output) { output.address == address })
}

pub fn get_input(ctx: ScriptContext, address: Address) -> Option<Input> {
  list.find(
    ctx.transaction.inputs,
    fn(input) { input.output.address == address },
  )
}

pub fn scripthash_address(scripthash: ByteArray) {
  Address {
    payment_credential: ScriptCredential(scripthash),
    stake_credential: None,
  }
}

pub fn validate_pool_deposit(
  ctx: ScriptContext,
  output_reference: OutputReference,
  datum: PoolDatum,
  redeemer: PoolDepositRedeemer,
) -> Bool {
  let validator_address = scripthash_address(#"ff")

  expect Some(pool_output) = get_output(ctx, validator_address)
  expect Some(pool_input) = get_input(ctx, validator_address)

  True
}

pub fn validate_pool_borrow(
  ctx: ScriptContext,
  output_reference: OutputReference,
  datum: PoolDatum,
  redeemer: PoolBorrowRedeemer,
) -> Bool {
  let validator_address = scripthash_address(#"ff")

  expect Some(pool_output) = get_output(ctx, validator_address)
  expect Some(pool_input) = get_input(ctx, validator_address)
  True
}

validator {
  fn pool_contract(datum: PoolDatum, redeemer: PoolRedeemer, ctx: ScriptContext) {
    when redeemer.action is {
      PoolWithdraw(_) -> True
      PoolDeposit(pool_deposit_redeemer) ->
        when ctx.purpose is {
          Spend(output_ref) ->
            validate_pool_deposit(ctx, output_ref, datum, pool_deposit_redeemer)
          _ -> False
        }
      PoolBorrow(pool_borrow_redeemer) ->
        when ctx.purpose is {
          Spend(output_ref) ->
            validate_pool_borrow(ctx, output_ref, datum, pool_borrow_redeemer)
          _ -> False
        }
    }
  }
}
