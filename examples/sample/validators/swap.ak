use sample
use sample/mint
use sample/spend
use aiken/builtin

const something = 5

pub type Redeemer {
  signer: ByteArray,
  amount: Int,
  other_thing: Redeemer,
}

pub type Reen {
  Buy1 { signer: ByteArray, amount: Int }
  Sell1
}

pub fn twice(f: fn(Int) -> Int, initial: Int) -> Int {
  f(f(initial))
}

pub fn add_one(value: Int) -> Int {
  value + 1
}

pub fn add_two(x: Int) -> Int {
  twice(add_one, x)
}

pub fn final_check(z: Int) {
  z < 4
}

pub fn incrementor(counter: Int, target: Int) -> Int {
  if counter == target {
    counter
  } else if counter > target {
    counter - target
  } else {
    incrementor(counter + 1, target)
  }
}

pub type Datum {
  Offer { prices: List(Int), asset_class: ByteArray, other_thing: Datum }
  Sell
  Hold(Int)
}

pub fn spend(datum: Datum, _rdmr: Nil, _ctx: Nil) -> Bool {
  when datum is {
    Sell -> sample.eqIntPlusOne(sample.incrementor(0, 8), 9)
    Offer {
      prices: p,
      asset_class: ac,
      other_thing: Offer {
        other_thing: Offer { asset_class: nested_signer, prices: amounts, .. },
        ..
      },
    } -> 1 == 1
    Offer { prices, other_thing: thing, .. } ->
      when prices is {
        [] -> True
        [a] -> True
        [a, b, c] -> True
        [a, b, c, d, ..e] -> sample.big_a == a

      }
    _ -> False
  }
}
// let Redeemer{ signer, amount: amount2, other_thing: Redeemer{ signer: nested_signer, ..}} = datum
// True
// when datum is {
//   Offer{ price: p, asset_class: ac, thing: thing, other_thing: Redeemer{ other_thing: Redeemer{ signer: nested_signer, amount: amount, ..}, .. } } -> True
//   _ -> False
// }
//  Offer{ prices, other_thing: thing, .. } -> {
//     when prices is {
//       [] -> True
//       [1] -> False
//       [a, b] -> True
//       [a, b, ..[a]] -> False
//     }
//   }
