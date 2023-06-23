use other

pub type AssetClass {
  policy: ByteArray,
  name: ByteArray,
}

pub type DatumOrc1 {
  oracle_parameters: OracleParametersd,
  token_a_amount: Int,
  token_b_amount: Int,
  expiration_time: Int,
  maturity_time: Int,
}

pub type OracleParametersd {
  pool_nft_cs: AssetClass,
  oracle_nft_cs: AssetClass,
  token_a_cs: AssetClass,
  token_b_cs: AssetClass,
}

test oracle1() {
  let x: Data =
    DatumOrc1 {
      oracle_parameters: OracleParametersd {
        pool_nft_cs: AssetClass { policy: #"", name: #"" },
        oracle_nft_cs: AssetClass { policy: #"", name: #"" },
        token_a_cs: AssetClass { policy: #"", name: #"" },
        token_b_cs: AssetClass { policy: #"", name: #"" },
      },
      token_a_amount: 0,
      token_b_amount: 0,
      expiration_time: 0,
      maturity_time: 0,
    }

  let y: Data = x

  expect _: other.DatumOrc = y

  // od == od
  True
}
