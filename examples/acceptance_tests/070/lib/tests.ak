use aiken/list
use aiken/transaction.{InlineDatum, Input, OutputReference, TransactionId}

type OtherInput {
  output_reference: OutputReference,
  other: Data,
}

type MyDatum<a> {
  Constructor1(a)
  Constructor2
}

test discard_partitions() {
  let all_inputs =
    [
      OtherInput(OutputReference(TransactionId(#"aabb"), 2), 3),
      OtherInput(OutputReference(TransactionId(#"aabbcc"), 3), 3),
    ]

  let own_out_ref = OutputReference(TransactionId(#"aabb"), 2)

  expect ([_], other_inputs) =
    list.partition(
      all_inputs,
      fn(input) { input.output_reference == own_out_ref },
    )

  let inputs: List<Input> =
    []

  list.all(
    inputs,
    fn(input) {
      expect dat: MyDatum<Int> =
        when input.output.datum is {
          InlineDatum(d) -> d
          _ -> fail @"Not an inline datum"
        }

      when dat is {
        Constructor1 { .. } -> True
        _ -> False
      }
    },
  )
}
