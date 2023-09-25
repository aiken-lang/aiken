type Schema {
  Integer(Int)
  List(List<Schema>)
  Constr(Int, Schema)
}

fn sum_constr(tag: Int, fields: Schema) -> Int {
  tag + sum(fields)
}

fn sum(schema: Schema) -> Int {
  when schema is {
    Integer(i) -> i
    List(xs) -> sum_list(xs)
    Constr(tag, fields) -> sum_constr(tag, fields)
  }
}

fn sum_list(list: List<Schema>) -> Int {
  when list is {
    [] -> 0
    [x, ..xs] -> sum(x) + sum_list(xs)
  }
}

test bar() {
  sum(List([List([Integer(1), Integer(2)]), Integer(3), Integer(4)])) == 10
}

fn prod(schema: Schema) -> Int {
  when schema is {
    Integer(i) -> i
    List(xs) -> prod_list(xs)
    Constr(tag, fields) -> prod_constr(tag, fields)
  }
}

fn prod_constr(tag: Int, fields: Schema) -> Int {
  tag * prod(fields) + sum(fields)
}

fn prod_list(list: List<Schema>) -> Int {
  when list is {
    [] -> 1
    [x, ..xs] -> prod(x) * prod_list(xs)
  }
}

test sum_prod() {
  prod(List([List([Integer(1), Integer(2)]), Integer(3), Integer(4)])) == 24
}
