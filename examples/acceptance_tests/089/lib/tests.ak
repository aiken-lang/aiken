use cardano/address.{Address, VerificationKey}
use cardano/assets.{from_lovelace}
use cardano/transaction.{Datum, InlineDatum, NoDatum, Output}

type SmallOracleState {
  Thing(Int)
}

fn create_output(
  payment_key: ByteArray,
  _thing,
  value: Int,
  other_thing,
  datum: Datum,
  script_ref: Option<ByteArray>,
) -> Output {
  let x =
    Output {
      address: Address {
        payment_credential: VerificationKey(payment_key),
        stake_credential: None,
      },
      value: from_lovelace(value),
      datum,
      reference_script: script_ref,
    }
  x
}

fn get_inline_datum(output: Output) -> Data {
  expect InlineDatum(datum) = output.datum
  datum
}

test get_inline_datum_test() {
  let output = create_output(#"00", True, 10, None, NoDatum, None)
  expect small: SmallOracleState = get_inline_datum(output)
  True
}
