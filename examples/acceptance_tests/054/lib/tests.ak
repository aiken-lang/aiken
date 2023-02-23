use aiken/transaction/value

test test_quantity_of_1() {
  let x = value.from_asset(#"000000", #"000020e05363726970744f776e6572", -1)
  value.quantity_of(x, #"000000", #"000020e05363726970744f776e6572") < 0
}
