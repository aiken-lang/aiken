use aiken/builtin

test drop_list() {
  builtin.drop_list(2, [1, 2, 3, 4]) == [3, 4]
}
