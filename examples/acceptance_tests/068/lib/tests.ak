use aiken/collection/dict.{Dict}
use aiken/crypto.{Blake2b_224, Hash, Script}
use aiken/primitive/bytearray

pub type PolicyId =
  Hash<Blake2b_224, Script>

pub type AssetName =
  ByteArray

pub opaque type Value {
  inner: Dict<PolicyId, Dict<AssetName, Int>>,
}

pub const zero: Value = Value { inner: dict.empty }

pub fn from_asset(
  policy_id: PolicyId,
  asset_name: AssetName,
  quantity: Int,
) -> Value {
  let asset =
    dict.empty
      |> dict.insert(asset_name, quantity)
  dict.empty
    |> dict.insert(policy_id, asset)
    |> Value
}

pub fn add(left v0: Value, right v1: Value) -> Value {
  dict.union_with(
    v0.inner,
    v1.inner,
    fn(_, a0, a1) {
      let result =
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

      if dict.is_empty(result) {
        None
      } else {
        Some(result)
      }
    },
  )
    |> Value
}

/// Flatten a value as a list of results, possibly discarding some along the way.
///
/// When the `transform` function returns `None`, the result is discarded altogether.
pub fn flatten_with(
  self: Value,
  transform: fn(PolicyId, AssetName, Int) -> Option<result>,
) -> List<result> {
  dict.foldr(
    self.inner,
    [],
    fn(policy_id, asset, assets) {
      dict.foldr(
        asset,
        assets,
        fn(asset_name, quantity, xs) {
          when transform(policy_id, asset_name, quantity) is {
            None -> xs
            Some(x) ->
              [x, ..xs]
          }
        },
      )
    },
  )
}

test flatten_with_1() {
  flatten_with(zero, fn(p, a, q) { Some((p, a, q)) }) == []
}

test flatten_with_2() {
  let v =
    zero
      |> add(from_asset("a", "1", 14))
      |> add(from_asset("b", "", 42))
      |> add(from_asset("a", "2", 42))

  flatten_with(
    v,
    fn(p, a, q) {
      if q == 42 {
        Some((p, a))
      } else {
        None
      }
    },
  ) == [("a", "2"), ("b", "")]
}
