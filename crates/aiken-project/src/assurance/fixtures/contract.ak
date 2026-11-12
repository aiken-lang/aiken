use bridge
use namespace

//@ PREDICATE
def accepted (n : Int) : Prop := positive n
@//

@onchain(steps: 100, semantics: e)
pub fn identity(value: Int) -> Int {
  value
}

@onchain(steps: 100, semantics: e)
validator policy {
  mint(_redeemer: Data, _policy_id: ByteArray, _ctx: Data) {
    True
  }
}

//@ PREDICATE
abbrev unchanged (n : Int) : Prop := same n n
@//

//@ PROPERTY mint_or_fallback for policy.mint policy.else identity
  "Mint, implicit fallback, and a function share an execution context."
  ∀ (n : Int), accepted n → unchanged n
@//
