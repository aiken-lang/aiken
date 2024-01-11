use aiken/list
use benchmarks/knights/heuristic.{descendants, finished_tour, start_tour}
use benchmarks/knights/types.{ChessSet, Solution}
use benchmarks/queue.{
  Queue, append_all_front, append_front, create_queue, head, is_empty,
  remove_front, to_list,
}

test run_knights0() {
  run_knights(0, 0) == []
}

test run_knights1() {
  run_knights(2, 2) == []
}

fn run_knights(depth: Int, board_size: Int) -> Solution {
  depth_search(depth, root(board_size), grow, is_fin) |> to_list
}

fn depth_search(
  depth: Int,
  queue: Queue<a>,
  grow_fn: fn(a) -> List<a>,
  fin_fn: fn(a) -> Bool,
) -> Queue<a> {
  if depth == 0 || is_empty(queue) {
    create_queue()
  } else if fin_fn(head(queue)) {
    depth_search(depth - 1, remove_front(queue), grow_fn, fin_fn)
      |> append_front(head(queue))
  } else {
    append_all_front(remove_front(queue), grow_fn(head(queue)))
      |> depth_search(depth - 1, _, grow_fn, fin_fn)
  }
}

fn root(sze: Int) -> Queue<(Int, ChessSet)> {
  append_all_front(create_queue(), mk_starts(sze))
}

fn mk_starts(sze: Int) -> List<(Int, ChessSet)> {
  let x_list = interval(1, sze)
  let y_list = interval(1, sze)

  let l = x_list |> list.map2(y_list, fn(a, b) { start_tour((a, b), sze) })
  let length = list.length(l)

  list.repeat(1 - length, length) |> list.zip(l)
}

fn interval(a: Int, b: Int) -> List<Int> {
  if a > b {
    []
  } else {
    [a, ..interval(a + 1, b)]
  }
}

fn grow(item: (Int, ChessSet)) -> List<(Int, ChessSet)> {
  let (x, y) = item

  let const_item = x + 1
  descendants(y) |> list.map(fn(list_item) { (const_item, list_item) })
}

fn is_fin(item: (Int, ChessSet)) -> Bool {
  let (_, y) = item

  finished_tour(y)
}
