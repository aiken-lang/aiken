use aiken/fuzz

test prop_option_distribution(opt via fuzz.option(fuzz.int())) {
  fuzz.label(
    when opt is {
      None -> @"None"
      Some(..) -> @"Some"
    },
  )
  True
}
