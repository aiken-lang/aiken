use aiken/builtin
use aiken/list
use benchmarks/knights/chess_set.{
  add_piece, create_board, delete_first, first_piece, is_square_free,
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

pub fn all_descend(board: ChessSet) -> List<ChessSet> {
  board
    |> possible_moves
    |> list.map(fn(direction) { move_knight(board, direction) })
}

fn move_knight(board: ChessSet, direction: Direction) -> ChessSet {
  todo
}

fn possible_moves(board: ChessSet) -> List<Direction> {
  todo
}
// data Direction = UL | UR | DL |DR | LU | LD | RU | RD

// {-# INLINABLE move #-}
// move :: Direction -> Tile -> Tile
// move UL (x,y) = (x-1,y-2)
// move UR (x,y) = (x+1,y-2)
// move DL (x,y) = (x-1,y+2)
// move DR (x,y) = (x+1,y+2)
// move LU (x,y) = (x-2,y-1)
// move LD (x,y) = (x-2,y+1)
// move RU (x,y) = (x+2,y-1)
// move RD (x,y) = (x+2,y+1)

// {-# INLINABLE startTour #-}
// startTour :: Tile -> Integer -> ChessSet
// startTour st size
//    | (size `Tx.remainder` 2) == 0 = createBoard size st
//    | otherwise           = {-Tx.trace "startTour" $ -} Tx.error ()

// {-# INLINABLE moveKnight #-}
// moveKnight :: ChessSet -> Direction -> ChessSet
// moveKnight board dir
//    = addPiece (move dir (lastPiece board)) board

// {-# INLINABLE canMove #-}
// canMove :: ChessSet -> Direction -> Bool
// canMove board dir
//    = canMoveTo (move dir (lastPiece board)) board

// {-# INLINABLE canMoveTo #-}
// canMoveTo :: Tile -> ChessSet -> Bool
// canMoveTo t@(x,y) board
//    = (x Tx.>= 1) && (x Tx.<= sze) &&
//      (y Tx.>= 1) && (y Tx.<= sze) &&
//      isSquareFree t board
//      where
//         sze = sizeBoard board

// {-# INLINABLE descendents #-}
// descendents :: ChessSet -> [ChessSet]
// descendents board =
//   if (canJumpFirst board) && (deadEnd (addPiece (firstPiece board) board))
//   then []
//   else
//       let l = Tx.length singles in
//       if l == 0 then map snd (quickSort (descAndNo board))
//       else if l == 1 then singles
//            else []           -- Going to be dead end
//                where
//                  singles = singleDescend board

// {-# INLINABLE singleDescend #-}
// singleDescend :: ChessSet -> [ChessSet]
// singleDescend board =[x | (y,x) <- descAndNo board, y==1]

// {-# INLINABLE descAndNo #-}
// descAndNo :: ChessSet -> [(Integer,ChessSet)]
// descAndNo board
//    = [(Tx.length (possibleMoves (deleteFirst x)),x) | x <- allDescend board]

// {-# INLINABLE allDescend #-}
// allDescend :: ChessSet -> [ChessSet]
// allDescend board
//    =  map (moveKnight board) (possibleMoves board)

// {-# INLINABLE possibleMoves #-}
// possibleMoves :: ChessSet -> [Direction]
// possibleMoves board
//    =[x | x <- [UL,UR,DL,DR,LU,LD,RU,RD], (canMove board x)]

// {-# INLINABLE deadEnd #-}
// deadEnd :: ChessSet -> Bool
// deadEnd board = (Tx.length (possibleMoves board)) == 0

// {-# INLINABLE canJumpFirst #-}
// canJumpFirst :: ChessSet -> Bool
// canJumpFirst board
//   = canMoveTo (firstPiece board) (deleteFirst board)

// {-# INLINABLE tourFinished #-}
// tourFinished :: ChessSet -> Bool
// tourFinished board
//    = (noPieces board == (sze*sze)) && (canJumpFirst board)
//      where
//         sze = sizeBoard board
