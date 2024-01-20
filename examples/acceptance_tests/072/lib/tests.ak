use aiken/transaction.{Datum, InlineDatum}

type OwnerInfo {
  pkh: ByteArray,
  sc: ByteArray,
}

type TokenInfo {
  pid: ByteArray,
  tkn: ByteArray,
  amt: Int,
}

type SwapInfo {
  slip: Int,
}

type OtherDatum {
  owner: OwnerInfo,
  have: TokenInfo,
  want: TokenInfo,
  info: SwapInfo,
}

type Output {
  datum: Datum,
}

fn find_outbound_datum(possible_output: Option<Output>) -> Data {
  when possible_output is {
    Some(possible_output) ->
      when possible_output.datum is {
        InlineDatum(outbound_datum) -> outbound_datum
        _ -> fail @"expected outbound inline datum"
      }
    None -> fail @"no outbound datum found"
  }
}

fn datum_a_cont() -> OtherDatum {
  let owner: OwnerInfo = OwnerInfo { pkh: #"", sc: #"" }
  let have: TokenInfo = TokenInfo { pid: #"", tkn: #"", amt: 100 }
  let want: TokenInfo = TokenInfo { pid: #"acab", tkn: #"beef", amt: 50 }
  let info: SwapInfo = SwapInfo { slip: 40 }
  OtherDatum { owner, have, want, info }
}

test foo() {
  let outbound_datum = InlineDatum(datum_a_cont())
  let outbound_output = Some(Output { datum: outbound_datum })
  expect outbound_datum: OtherDatum = find_outbound_datum(outbound_output)
  outbound_datum == datum_a_cont()
}
