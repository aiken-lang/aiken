type Schema {
  Integer(Int)
  List(List<Schema>)
}

fn sum(schema: Schema) -> Int {
  when schema is {
    Integer(i) -> i
    List(xs) -> sum_list(xs)
  }
}

fn sum_list(list: List<Schema>) -> Int {
  when list is {
    [] -> 0
    [x, ..xs] -> sum(x) + sum_list(xs)
  }
}

test foo() {
  sum(List([List([Integer(1), Integer(2)]), Integer(3), Integer(4)])) == 10
}
