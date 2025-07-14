use cardano/address.{Address, VerificationKey as VerificationKeyContstructor}

test other() {
  let address = Address(VerificationKeyContstructor(#"affdafafaa"), None)

  when address.payment_credential is {
    VerificationKeyContstructor(thing) -> #"affdafafaa" == thing
    _ -> fail
  }
}
