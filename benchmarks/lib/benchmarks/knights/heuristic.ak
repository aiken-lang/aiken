use aiken/builtin
use aiken/collection/list
use aiken/primitive/int
use benchmarks/knights/chess_set.{
  add_piece, create_board, delete_first, first_piece, is_square_free, last_piece,
}
use benchmarks/knights/sort.{quicksort}
use benchmarks/knights/types.{ChessSet, Tile}

type Direction {
  UL
  UR
  DL
  DR
  LU
  LD
  RU
  RD
}

const direction_list = [UL, UR, DL, DR, LU, LD, RU, RD]

fn move(direction: Direction, tile: Tile) -> Tile {
  let (x, y) = tile
  when direction is {
    UL -> (x - 1, y - 2)
    UR -> (x + 1, y - 2)
    DL -> (x - 1, y + 2)
    DR -> (x + 1, y + 2)
    LU -> (x - 2, y - 1)
    LD -> (x - 2, y + 1)
    RU -> (x + 2, y - 1)
    RD -> (x + 2, y + 1)
  }
}

pub fn start_tour(st: Tile, size: Int) -> ChessSet {
  expect 0 = builtin.remainder_integer(size, 2)
  create_board(size, st)
}

pub fn tour_finished(board: ChessSet) -> Bool {
  let ChessSet { move_number, size, .. } = board
  move_number == size * size && can_jump_first(board)
}

pub fn descendants(board: ChessSet) -> List<ChessSet> {
  if and {
    can_jump_first(board),
    board
      |> first_piece
      |> add_piece(board, _)
      |> dead_end,
  } {
    []
  } else {
    let singles = single_descend(board)

    when singles is {
      [] ->
        board
          |> desc_and_no
          |> quicksort(compare_chess_set)
          |> list.map(fn(t) { t.2nd })
      [_] -> singles
      _ -> []
    }
  }
}

pub fn can_jump_first(board: ChessSet) -> Bool {
  can_move_to(delete_first(board), first_piece(board))
}

pub fn dead_end(board: ChessSet) -> Bool {
  board |> possible_moves |> builtin.null_list
}

pub fn single_descend(board: ChessSet) -> List<ChessSet> {
  list.filter_map(
    desc_and_no(board),
    fn(item) {
      let (moves, board) = item
      if moves == 1 {
        Some(board)
      } else {
        None
      }
    },
  )
}

pub fn desc_and_no(board: ChessSet) -> List<(Int, ChessSet)> {
  board
    |> all_descend
    |> list.map(
        fn(item) {
          (item |> delete_first |> possible_moves |> list.length, item)
        },
      )
}

pub fn can_move_to(board: ChessSet, tile: Tile) -> Bool {
  let (x, y) = tile
  let size = board.size
  and {
    x >= 1,
    x <= size,
    y >= 1,
    y <= size,
    is_square_free(board, tile),
  }
}

fn can_move(board: ChessSet, direction: Direction) -> Bool {
  board |> can_move_to(move(direction, last_piece(board)))
}

pub fn all_descend(board: ChessSet) -> List<ChessSet> {
  board
    |> possible_moves
    |> list.map(move_knight(board, _))
}

fn move_knight(board: ChessSet, direction: Direction) -> ChessSet {
  add_piece(board, move(direction, last_piece(board)))
}

fn possible_moves(board: ChessSet) -> List<Direction> {
  direction_list |> list.filter(can_move(board, _))
}

fn compare_chess_set(a: (Int, ChessSet), b: (Int, ChessSet)) -> Ordering {
  if a.1st != b.1st {
    int.compare(a.1st, b.1st)
  } else {
    compare_list(a.2nd.visited, b.2nd.visited)
  }
}

fn compare_list(xs: List<Tile>, ys: List<Tile>) -> Ordering {
  when xs is {
    [] ->
      when ys is {
        [] -> Equal
        _ -> Less
      }
    [x, ..xs] ->
      when ys is {
        [] -> Greater
        [y, ..ys] -> {
          let ord = compare_tile(x, y)
          if ord == Equal {
            compare_list(xs, ys)
          } else {
            ord
          }
        }
      }
  }
}

fn compare_tile(a: Tile, b: Tile) -> Ordering {
  if a.1st == b.1st {
    int.compare(a.2nd, b.2nd)
  } else {
    int.compare(a.1st, b.1st)
  }
}
