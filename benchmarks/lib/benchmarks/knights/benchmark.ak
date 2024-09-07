use aiken/collection/list
use benchmarks/knights/heuristic.{descendants, start_tour, tour_finished}
use benchmarks/knights/queue.{Queue}
use benchmarks/knights/types.{ChessSet, Solution}

// ------------------------------------------------------------------ Benchmarks

test bench_knights_100_4x4() {
  run_knights(100, 4) == []
}

test bench_knights_100_6x6() {
  run_knights(100, 6) == solution_100_6x6
}

test bench_knights_100_8x8() {
  run_knights(100, 8) == solution_100_8x8
}

// ----------------------------------------------------------------------- Setup

fn run_knights(depth: Int, board_size: Int) -> Solution {
  depth_search(depth, root(board_size), grow, done) |> queue.to_list
}

fn depth_search(
  depth: Int,
  xs: Queue<a>,
  grow: fn(a) -> List<a>,
  done: fn(a) -> Bool,
) -> Queue<a> {
  if depth == 0 || queue.is_empty(xs) {
    queue.empty
  } else if done(queue.head(xs)) {
    depth_search(depth - 1, queue.remove_front(xs), grow, done)
      |> queue.append_front(queue.head(xs))
  } else {
    queue.append_all_front(queue.remove_front(xs), grow(queue.head(xs)))
      |> depth_search(depth - 1, _, grow, done)
  }
}

fn root(size: Int) -> Queue<(Int, ChessSet)> {
  queue.append_all_front(queue.empty, mk_starts(size))
}

fn mk_starts(size: Int) -> List<(Int, ChessSet)> {
  let it = interval(1, size)

  let l =
    list.flat_map(
      it,
      fn(x) { list.map(it, fn(y) { start_tour((x, y), size) }) },
    )

  let length = list.length(l)

  expect length == size * size

  list.zip(list.repeat(1 - length, length), l)
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
  descendants(y) |> list.map(fn(list_item) { (x + 1, list_item) })
}

fn done(item: (Int, ChessSet)) -> Bool {
  tour_finished(item.2nd)
}

// ------------------------------------------------------------------ Fixtures

const solution_100_6x6: Solution =
  [
    (
      0,
      ChessSet {
        size: 6,
        move_number: 36,
        start: Some((1, 1)),
        visited: [
          (3, 2), (5, 3), (6, 1), (4, 2), (3, 4), (2, 6), (4, 5), (6, 6), (5, 4),
          (6, 2), (4, 1), (2, 2), (1, 4), (3, 3), (2, 1), (1, 3), (2, 5), (4, 6),
          (6, 5), (4, 4), (5, 2), (6, 4), (5, 6), (3, 5), (1, 6), (2, 4), (1, 2),
          (3, 1), (4, 3), (5, 1), (6, 3), (5, 5), (3, 6), (1, 5), (2, 3), (1, 1),
        ],
      },
    ),
    (
      0,
      ChessSet {
        size: 6,
        move_number: 36,
        start: Some((1, 1)),
        visited: [
          (3, 2), (5, 3), (6, 1), (4, 2), (3, 4), (2, 2), (4, 1), (6, 2), (5, 4),
          (6, 6), (4, 5), (2, 6), (1, 4), (3, 3), (2, 1), (1, 3), (2, 5), (4, 6),
          (6, 5), (4, 4), (5, 2), (6, 4), (5, 6), (3, 5), (1, 6), (2, 4), (1, 2),
          (3, 1), (4, 3), (5, 1), (6, 3), (5, 5), (3, 6), (1, 5), (2, 3), (1, 1),
        ],
      },
    ),
    (
      0,
      ChessSet {
        size: 6,
        move_number: 36,
        start: Some((1, 1)),
        visited: [
          (3, 2), (5, 3), (6, 1), (4, 2), (3, 4), (2, 2), (1, 4), (2, 6), (4, 5),
          (6, 6), (5, 4), (6, 2), (4, 1), (3, 3), (2, 1), (1, 3), (2, 5), (4, 6),
          (6, 5), (4, 4), (5, 2), (6, 4), (5, 6), (3, 5), (1, 6), (2, 4), (1, 2),
          (3, 1), (4, 3), (5, 1), (6, 3), (5, 5), (3, 6), (1, 5), (2, 3), (1, 1),
        ],
      },
    ),
    (
      0,
      ChessSet {
        size: 6,
        move_number: 36,
        start: Some((1, 1)),
        visited: [
          (3, 2), (5, 3), (6, 1), (4, 2), (3, 4), (2, 6), (1, 4), (2, 2), (4, 1),
          (6, 2), (5, 4), (6, 6), (4, 5), (3, 3), (2, 1), (1, 3), (2, 5), (4, 6),
          (6, 5), (4, 4), (5, 2), (6, 4), (5, 6), (3, 5), (1, 6), (2, 4), (1, 2),
          (3, 1), (4, 3), (5, 1), (6, 3), (5, 5), (3, 6), (1, 5), (2, 3), (1, 1),
        ],
      },
    ),
  ]

const solution_100_8x8: Solution =
  [
    (
      0,
      ChessSet {
        size: 8,
        move_number: 64,
        start: Some((1, 1)),
        visited: [
          (3, 2), (4, 4), (5, 6), (6, 4), (8, 5), (7, 7), (6, 5), (8, 4), (7, 2),
          (5, 3), (3, 4), (4, 6), (5, 8), (6, 6), (4, 5), (3, 7), (1, 8), (2, 6),
          (4, 7), (5, 5), (6, 3), (5, 1), (4, 3), (3, 5), (5, 4), (7, 3), (8, 1),
          (6, 2), (4, 1), (2, 2), (1, 4), (3, 3), (2, 5), (1, 3), (2, 1), (4, 2),
          (6, 1), (8, 2), (7, 4), (8, 6), (7, 8), (5, 7), (3, 8), (1, 7), (3, 6),
          (2, 8), (1, 6), (2, 4), (1, 2), (3, 1), (5, 2), (7, 1), (8, 3), (7, 5),
          (8, 7), (6, 8), (7, 6), (8, 8), (6, 7), (4, 8), (2, 7), (1, 5), (2, 3),
          (1, 1),
        ],
      },
    ),
    (
      0,
      ChessSet {
        size: 8,
        move_number: 64,
        start: Some((1, 1)),
        visited: [
          (3, 2), (4, 4), (5, 6), (7, 7), (8, 5), (6, 4), (7, 2), (8, 4), (6, 5),
          (5, 3), (3, 4), (4, 6), (5, 8), (6, 6), (4, 5), (3, 7), (1, 8), (2, 6),
          (4, 7), (5, 5), (6, 3), (5, 1), (4, 3), (3, 5), (5, 4), (7, 3), (8, 1),
          (6, 2), (4, 1), (2, 2), (1, 4), (3, 3), (2, 5), (1, 3), (2, 1), (4, 2),
          (6, 1), (8, 2), (7, 4), (8, 6), (7, 8), (5, 7), (3, 8), (1, 7), (3, 6),
          (2, 8), (1, 6), (2, 4), (1, 2), (3, 1), (5, 2), (7, 1), (8, 3), (7, 5),
          (8, 7), (6, 8), (7, 6), (8, 8), (6, 7), (4, 8), (2, 7), (1, 5), (2, 3),
          (1, 1),
        ],
      },
    ),
    (
      0,
      ChessSet {
        size: 8,
        move_number: 64,
        start: Some((1, 1)),
        visited: [
          (3, 2), (4, 4), (6, 5), (8, 4), (7, 2), (5, 3), (3, 4), (4, 6), (5, 8),
          (7, 7), (5, 6), (6, 4), (8, 5), (6, 6), (4, 5), (3, 7), (1, 8), (2, 6),
          (4, 7), (5, 5), (6, 3), (5, 1), (4, 3), (3, 5), (5, 4), (7, 3), (8, 1),
          (6, 2), (4, 1), (2, 2), (1, 4), (3, 3), (2, 5), (1, 3), (2, 1), (4, 2),
          (6, 1), (8, 2), (7, 4), (8, 6), (7, 8), (5, 7), (3, 8), (1, 7), (3, 6),
          (2, 8), (1, 6), (2, 4), (1, 2), (3, 1), (5, 2), (7, 1), (8, 3), (7, 5),
          (8, 7), (6, 8), (7, 6), (8, 8), (6, 7), (4, 8), (2, 7), (1, 5), (2, 3),
          (1, 1),
        ],
      },
    ),
  ]
