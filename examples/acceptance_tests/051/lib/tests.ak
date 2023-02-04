use aiken/builtin

test ecdsa() {
  let public_key =
    #"02c66e7d8966b5c555af5805989da9fbf8db95e15631ce358c3a1710c962679063"

  let message =
    #"aadf7de782034fbe3d3db2cb13c0cd91bf41cb08fac7bd61d54453cf6e82b450"

  let signature =
    #"dc4dc264a9fef17a3f253449cf8c397ab6f16fb3d63d86940b5586823dfd02ae3b461bb4336b5ecbaefd6627aa922efc048fec0c881c10c4c9428fca69c132a2"

  builtin.verify_ecdsa_secp256k1_signature(public_key, message, signature)
}

test schnorr() {
  let public_key =
    #"b33cc9edc096d0a83416964bd3c6247b8fecd256e4efa7870d2c854bdeb33390"

  let message =
    #"e48441762fb75010b2aa31a512b62b4148aa3fb08eb0765d76b252559064a614"

  let signature =
    #"6470fd1303dda4fda717b9837153c24a6eab377183fc438f939e0ed2b620e9ee5077c4a8b8dca28963d772a94f5f0ddf598e1c47c137f91933274c7c3edadce8"

  builtin.verify_schnorr_secp256k1_signature(public_key, message, signature)
}
