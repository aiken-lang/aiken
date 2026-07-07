use aiken/builtin

test exp_mod_integer() {
  builtin.exp_mod_integer(4, 13, 497) == 445
}
