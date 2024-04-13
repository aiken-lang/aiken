use aiken/list

pub opaque type Queue<a> {
  inner: List<a>,
}

pub fn create_queue() -> Queue<a> {
  [] |> Queue
}

pub fn to_list(queue: Queue<a>) -> List<a> {
  queue.inner
}

pub fn is_empty(queue: Queue<a>) -> Bool {
  when queue.inner is {
    [] -> True
    _ -> False
  }
}

pub fn append_front(queue: Queue<a>, item: a) -> Queue<a> {
  list.push(queue.inner, item) |> Queue
}

/// Add all items from the list to the front of the queue
pub fn append_all_front(queue: Queue<a>, items: List<a>) -> Queue<a> {
  list.concat(items, queue.inner) |> Queue
}

pub fn remove_front(queue: Queue<a>) -> Queue<a> {
  expect [_, ..rest] = queue.inner

  rest |> Queue
}

pub fn head(queue: Queue<a>) -> a {
  expect [q, ..] = queue.inner

  q
}
