fn helper_0(value: Int) -> Int {
  value + 1
}

fn helper_1(value: Int) -> Int {
  helper_0(value) + 1
}

fn helper_2(value: Int) -> Int {
  helper_1(value) + 1
}

fn helper_3(value: Int) -> Int {
  helper_2(value) + 1
}

const shared_value: Int = helper_3(0)

test shared_graph_0() {
  shared_value == 4
}

test shared_graph_1() {
  shared_value == 4
}

test shared_graph_2() {
  shared_value == 4
}

test shared_graph_3() {
  shared_value == 4
}
