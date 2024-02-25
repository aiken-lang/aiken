use aiken/builtin

test u32_boundary_down() {
  builtin.serialise_data(0xdeadbeefdeadbeef) == #"1bdeadbeefdeadbeef"
}

test u32_boundary_up() {
  builtin.serialise_data(-0xdeadbeefdeadbeef) == #"3bdeadbeefdeadbeee"
}
