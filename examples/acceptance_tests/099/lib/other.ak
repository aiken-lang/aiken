pub type PublicKeyHash =
  ByteArray

pub type ByteArrayDataPair {
  key: ByteArray,
  value: Data,
}

pub type ReferenceDatum {
  data: List<ByteArrayDataPair>,
  controller: PublicKeyHash,
}

pub fn find_data(datas: List<ByteArrayDataPair>, key: ByteArray) -> Data {
  when datas is {
    [] -> fail @"Data Structure Not Found"
    [d, ..ds] ->
      if d.key == key {
        d.value
      } else {
        find_data(ds, key)
      }
  }
}

test thing() {
  let stuff: Data =
    ReferenceDatum([ByteArrayDataPair("key", "value")], "controller")

  expect other: ReferenceDatum = stuff
  other == other
}
