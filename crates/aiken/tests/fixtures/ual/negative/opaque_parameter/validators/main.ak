opaque type Secret {
  value: Int,
}

@onchain(steps: 100, semantics: e)
fn target(value: Secret) -> Secret {
  value
}

//@ PROPERTY opaque_parameter for target "Opaque interfaces are not exportable." True @//
