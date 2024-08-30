use aiken/collection/list
use benchmarks/knights/types.{ChessSet, Tile}

pub fn create_board(size: Int, init_square: Tile) -> ChessSet {
  ChessSet {
    size,
    move_number: 1,
    start: Some(init_square),
    visited: [init_square],
  }
}

pub fn add_piece(board: ChessSet, tile: Tile) -> ChessSet {
  ChessSet {
    ..board,
    move_number: board.move_number + 1,
    visited: [tile, ..board.visited],
  }
}

pub fn first_piece(board: ChessSet) -> Tile {
  expect Some(tile) = board.start
  tile
}

pub fn last_piece(board: ChessSet) -> Tile {
  when board.visited is {
    [] -> fail
    [x, ..] -> x
  }
}

pub fn delete_first(board: ChessSet) -> ChessSet {
  let ChessSet { move_number, visited, .. } = board

  expect Some(new_visited) = list.init(visited)

  ChessSet {
    ..board,
    move_number: move_number - 1,
    start: second_last(visited),
    visited: new_visited,
  }
}

fn second_last(visited: List<a>) -> Option<a> {
  when list.reverse(visited) is {
    [] -> fail
    [_] -> None
    [_, a, ..] -> Some(a)
  }
}

pub fn is_square_free(board: ChessSet, tile: Tile) -> Bool {
  !list.has(board.visited, tile)
}
