use aiken/collection/list

pub opaque type Queue<a> {
  inner: List<a>,
}

pub const empty: Queue<a> = [] |> Queue

pub fn to_list(self: Queue<a>) -> List<a> {
  self.inner
}

pub fn is_empty(self: Queue<a>) -> Bool {
  when self.inner is {
    [] -> True
    _ -> False
  }
}

pub fn append_front(self: Queue<a>, item: a) -> Queue<a> {
  list.push(self.inner, item) |> Queue
}

pub fn append_all_front(self: Queue<a>, items: List<a>) -> Queue<a> {
  list.concat(items, self.inner) |> Queue
}

pub fn remove_front(self: Queue<a>) -> Queue<a> {
  expect [_, ..rest] = self.inner
  rest |> Queue
}

pub fn head(self: Queue<a>) -> a {
  expect [q, ..] = self.inner
  q
}
