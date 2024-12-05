use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}

pub type StateMachineDatum {
  state: Int,
  buyer: ByteArray,
  seller: ByteArray,
  collateral: Int,
  price: Int,
  accept_range: Int,
}

pub type StateMachineInput {
  Return
  Other
}

validator statemachine(threadtoken: PolicyId) {
  spend(
    datum_opt: Option<StateMachineDatum>,
    redeemer: StateMachineInput,
    own_ref: OutputReference,
    transaction: Transaction,
  ) {
    expect Some(datum) = datum_opt
    when (datum, redeemer) is {
      (
        StateMachineDatum {
          state,
          buyer,
          seller,
          price,
          collateral,
          accept_range,
        },
        Return,
      ) -> {
        let must_be_state = state == 0

        let must_be_signed = list.has(transaction.extra_signatories, buyer)
        //One of the transaction inputs belongs to the statemachine.
        expect Some(sm_input) =
          list.find(
            transaction.inputs,
            fn(input) { input.output_reference == own_ref },
          )
        //One of the transaction outputs contains the threadtoken addressed to the statemachine itself - 1.
        expect Some(sm_output) =
          list.find(
            transaction.outputs,
            fn(output) { output.address == sm_input.output.address },
          )
        //One of the transaction outputs contains the threadtoken addressed to the statemachine itself - 2.
        let must_be_policy =
          list.has(assets.policies(sm_output.value), threadtoken)

        //verification of the new datum - 1.
        let new_data: Data =
          StateMachineDatum {
            state: -1,
            buyer,
            seller,
            collateral,
            price,
            accept_range,
          }
        //verification of the new datum - 2.
        let must_be_datum = InlineDatum(new_data) == sm_output.datum

        and {
          must_be_state?,
          must_be_signed?,
          must_be_policy?,
          must_be_datum?,
        }
      }

      _ -> False
    }
  }

  else(_) {
    fail
  }
}
