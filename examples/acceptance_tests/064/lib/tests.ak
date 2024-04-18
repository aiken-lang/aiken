use aiken/builtin

type Foo {
  A(Int)
  B(Int, Int)
}

fn get_constr(data: Data) -> Int {
  builtin.un_constr_data(data).1st
}

test foo() {
  get_constr(A(42)) == 0 && get_constr(B(14, 42)) == 1
}

fn map(list: List<a>, f: fn(a) -> b) -> List<b> {
  when list is {
    [] ->
      []
    [x, ..xs] ->
      [f(x), ..map(xs, f)]
  }
}

fn get_fields(data: Data) -> List<Int> {
  builtin.un_constr_data(data).2nd
    |> map(builtin.un_i_data)
}

test bar() {
  get_fields(A(42)) == [42] && get_fields(B(14, 42)) == [14, 42]
}
