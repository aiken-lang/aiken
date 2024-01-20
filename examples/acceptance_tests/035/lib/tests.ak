use aiken/dict.{Dict}

pub type PolicyId =
  ByteArray

pub type AssetName =
  ByteArray

pub type Value =
  Dict<PolicyId, Dict<AssetName, Int>>

pub fn from_asset(
  policy_id: PolicyId,
  asset_name: AssetName,
  quantity: Int,
) -> Value {
  let asset =
    dict.new()
      |> dict.insert(asset_name, quantity)
  dict.new()
    |> dict.insert(policy_id, asset)
}

pub fn from_lovelace(quantity: Int) -> Value {
  from_asset(#"", #"", quantity)
}

pub fn add(left v0: Value, right v1: Value) -> Value {
  dict.union_with(
    v0,
    v1,
    fn(_, a0, a1) {
      let asset =
        dict.union_with(
          a0,
          a1,
          fn(_, q0, q1) {
            let q = q0 + q1
            if q == 0 {
              None
            } else {
              Some(q)
            }
          },
        )

      when dict.toList(asset) is {
        [] -> None
        _ -> Some(asset)
      }
    },
  )
}

test add_1() {
  let v1 = from_lovelace(1)
  let v2 = from_lovelace(-1)
  add(v1, v2) == dict.new()
}
