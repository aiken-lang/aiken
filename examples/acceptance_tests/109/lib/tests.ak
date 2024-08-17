use config

test config_int() {
  config.int == 42
}

test config_bool() {
  config.bool == True
}

test config_string() {
  config.string == "foo"
}

test config_bytearray() {
  config.bytearray == "foo"
}

test config_tuple() {
  config.tuple == (14, False)
}

test config_list() {
  config.list == [1, 2, 3, 4, 5, 6]
}

test config_nested_tuple() {
  config.nested_tuple == ((True, "foo"), (1, []))
}

test config_nested_list() {
  config.nested_list == [[1, 2], [3, 4]]
}

test config_nested_hybrid() {
  config.nested_hybrid == [(1, True), (2, False)]
}
