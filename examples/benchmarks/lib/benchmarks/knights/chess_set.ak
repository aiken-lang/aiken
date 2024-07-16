use aiken/list
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

  expect Some(deleted_first) = list.init(visited)

  ChessSet {
    ..board,
    move_number: move_number - 1,
    start: second_last(visited),
    visited: deleted_first,
  }
}

fn second_last(visited: List<a>) -> Option<a> {
  when visited is {
    [] -> None
    [_, ..rest] -> {
      let value = second_last(rest)

      if value == None {
        if builtin.null_list(rest) {
          None
        } else {
          Some(builtin.head_list(visited))
        }
      } else {
        value
      }
    }
  }
}

pub fn is_square_free(board: ChessSet, tile: Tile) -> Bool {
  !list.has(board.visited, tile)
}
