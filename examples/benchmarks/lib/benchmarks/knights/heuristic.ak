use aiken/builtin
use aiken/list
use benchmarks/knights/chess_set.{
  add_piece, create_board, delete_first, first_piece, is_square_free, last_piece,
}
use benchmarks/knights/sort.{quick_sort}
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

fn direction_list() {
  [UL, UR, DL, DR, LU, LD, RU, RD]
}

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

pub fn finished_tour(board: ChessSet) -> Bool {
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
      [] -> board |> desc_and_no |> quick_sort |> list.map(builtin.snd_pair)
      [_] -> singles
      _ ->
        []
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
  let descendants_and_moves = desc_and_no(board)
  list.filter_map(
    descendants_and_moves,
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
    |> list.map(fn(direction) { move_knight(board, direction) })
}

fn move_knight(board: ChessSet, direction: Direction) -> ChessSet {
  add_piece(board, move(direction, last_piece(board)))
}

fn possible_moves(board: ChessSet) -> List<Direction> {
  direction_list() |> list.filter(fn(direction) { can_move(board, direction) })
}
