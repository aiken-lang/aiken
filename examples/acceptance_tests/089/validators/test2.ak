use aiken/list
use aiken/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, ScriptContext, Transaction,
  TransactionId,
}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value

pub const own_hash = #"01020304050607080910111213140102030405060708091011121314"

pub const other_hash =
  #"02030405060708091011121314150203040506070809101112131415"

pub const beneficiary_keyhash =
  #"03040506070809101112131415160304050607080910111213141516"

pub fn beneficiary_address() {
  keyhash_address(beneficiary_keyhash)
}

pub fn keyhash_address(keyhash: ByteArray) {
  Address {
    payment_credential: VerificationKeyCredential(keyhash),
    stake_credential: None,
  }
}

validator(utxo_ref: OutputReference) {
  fn simple_oneshot(_r: Void, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    let Transaction { inputs, .. } = transaction

    expect Mint(_policy_id) = purpose

    expect Some(_input) =
      list.find(inputs, fn(input) { input.output_reference == utxo_ref })

    let input =
      list.find(inputs, fn(input) { input.output_reference == utxo_ref })
    when input is {
      Some(_) -> True
      None -> False
    }
  }
}

test test_simple_oneshot() {
  let output =
    Output {
      address: beneficiary_address(),
      value: value.zero(),
      datum: NoDatum,
      reference_script: None,
    }

  let val =
    value.from_asset(policy_id: own_hash, asset_name: "testtoken", quantity: 1)
  let utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }

  let input = Input { output_reference: utxo, output }

  let tx =
    Transaction {
      ..transaction.placeholder(),
      mint: value.to_minted_value(val),
      extra_signatories: [other_hash],
      inputs: [input],
    }
  let context = ScriptContext { purpose: Mint(own_hash), transaction: tx }
  simple_oneshot(utxo, Void, context) == True
}
