use aiken/bytearray
use aiken/dict.{Dict}
use aiken/int
use aiken/list
use aiken/transaction.{OutputReference, TransactionId}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{AssetName, PolicyId, Value}

fn compare_out_ref(ref1: OutputReference, ref2: OutputReference) -> Ordering {
  let OutputReference(TransactionId(tx_id1), out_index1) = ref1
  let OutputReference(TransactionId(tx_id2), out_index2) = ref2
  when bytearray.compare(tx_id1, tx_id2) is {
    Less -> Less
    Greater -> Greater
    Equal -> int.compare(out_index1, out_index2)
  }
}

// Compare two values based on a specific asset. Ordering doesn't matter; they just shouldn't
// be equal.
fn compare_value(
  loan_id: AssetName,
  sym: PolicyId,
  val1: Value,
  val2: Value,
) -> Ordering {
  if
  value.quantity_of(val1, sym, loan_id) == value.quantity_of(val2, sym, loan_id){
  
    fail @"Should not be equal"
  } else {
    Less
  }
}

test dict_test1() {
  let offer_input_ref = OutputReference(TransactionId("00"), 0)
  let ask_input_ref = OutputReference(TransactionId("00"), 1)
  let pairings =
    [(ask_input_ref, offer_input_ref)]

  let (ask_map, asize, offer_map, osize) =
    (
      [Pair(ask_input_ref, transaction.NoDatum)],
      1,
      [Pair(offer_input_ref, transaction.NoDatum)],
      1,
    )

  (ask_map, asize, offer_map, osize) == (
    [Pair(ask_input_ref, transaction.NoDatum)],
    1,
    [Pair(offer_input_ref, transaction.NoDatum)],
    1,
  )
}

test dict_test2() {
  let offer_input_ref = OutputReference(TransactionId("00"), 0)
  let ask_input_ref = OutputReference(TransactionId("00"), 1)
  let pairings =
    [(ask_input_ref, offer_input_ref)]

  let foo =
    fn(pair: (OutputReference, OutputReference), acc: Dict<ByteArray, Address>) {
      let new_pay_map =
        dict.insert(acc, "", Address(VerificationKeyCredential("00"), None))

      new_pay_map
    }

  let pay_map = list.foldl(pairings, dict.new(), foo)

  pay_map != dict.new()
}

test dict_test3() {
  let offer_input_ref = OutputReference(TransactionId("00"), 0)
  let ask_input_ref = OutputReference(TransactionId("00"), 1)
  let pairings =
    [(ask_input_ref, offer_input_ref)]

  let (ask_map, asize, offer_map, osize) =
    (
      [Pair(ask_input_ref, transaction.NoDatum)],
      1,
      [Pair(offer_input_ref, transaction.NoDatum)],
      1,
    )

  // TODO: Maybe passing Value to the key generic of dict shouldn't be possible
  let foo =
    fn(pair: (OutputReference, OutputReference), acc: Dict<Value, Address>) {
      let new_pay_map =
        dict.insert(acc, "", Address(VerificationKeyCredential("00"), None))

      new_pay_map
    }

  let pay_map = list.foldl(pairings, dict.new(), foo)

  pay_map != dict.new()
}
