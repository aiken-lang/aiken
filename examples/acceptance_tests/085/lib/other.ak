pub type AssetClass {
  policy: ByteArray,
  name: ByteArray,
}

pub type DatumOrc {
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
