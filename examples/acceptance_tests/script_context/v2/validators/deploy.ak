use aiken/builtin
use aiken/dict.{Dict}
use aiken/hash.{blake2b_256}
use aiken/transaction.{DatumHash, InlineDatum, Output, ScriptContext}
use aiken/transaction/credential.{Inline, VerificationKeyCredential}

validator {
  fn spend(datum: Data, _redeemer: Data, ctx: ScriptContext) {
    and {
      assert_datum(datum),
      assert_datums(ctx.transaction.datums),
      assert_outputs(ctx.transaction.outputs),
    }
  }
}

fn assert_datum(datum) {
  let my_datum: Data = Void
  datum == my_datum
}

type MyDatum {
  MyDatum(Int)
}

fn assert_datums(datums: Dict<ByteArray, Data>) {
  let my_datum = MyDatum(42)

  expect Some(datum) =
    dict.get(datums, blake2b_256(builtin.serialise_data(my_datum)))

  expect datum: MyDatum = datum

  my_datum == datum && dict.size(datums) == 2
}

fn assert_outputs(outputs) {
  when outputs is {
    [output_1, output_2, ..] ->
      assert_first_output(output_1) && assert_second_output(output_2)
    _ -> fail @"expected transaction to have (at least) 2 outputs"
  }
}

fn assert_first_output(output: Output) {
  and {
    output.datum == DatumHash(
      #"fcaa61fb85676101d9e3398a484674e71c45c3fd41b492682f3b0054f4cf3273",
    ),
    output.reference_script == Some(
      #"e37db487fbd58c45d059bcbf5cd6b1604d3bec16cf888f1395a4ebc4",
    ),
  }
}

fn assert_second_output(output: Output) {
  and {
    output.address.stake_credential == Some(
      Inline(
        VerificationKeyCredential(
          #"66666666666666666666666666666666666666666666666666666666",
        ),
      ),
    ),
    when output.datum is {
      InlineDatum(_) -> True
      _ -> fail @"expected inline datum"
    },
  }
}
