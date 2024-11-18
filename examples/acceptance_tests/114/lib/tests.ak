test baz() {
  let assets =
    [
      ("PolicyId1", [("AssetName", 20)]),
      ("PolicyId2", [("AssetName1", 300), ("AssetName2", 4_000)]),
      ("PolicyId3", []),
    ]
  let is_empty_or_contains_policyid2 =
    when assets is {
      [] -> True
      [_head, (p, [_, (a, q), ..] as tokens), ..] -> and {
          tokens == [("AssetName1", 300), ("AssetName2", 4000)],
          p == "PolicyId2",
          a == "AssetName2",
          q >= 4_000,
        }
      _ -> False
    }
  is_empty_or_contains_policyid2
}
