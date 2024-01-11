use aiken/builtin
use aiken/list
use benchmarks/knights/chess_set.{add_piece, create_board, first_piece}
use benchmarks/knights/sort.{quick_sort}
use benchmarks/knights/types.{ChessSet, Tile}

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
      |> add_piece(first_piece(board))
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
  todo
}

pub fn dead_end(board: ChessSet) -> Bool {
  todo
}

pub fn single_descend(board: ChessSet) -> List<ChessSet> {
  todo
}

pub fn desc_and_no(board: ChessSet) -> List<(Int, ChessSet)> {
  todo
}

pub fn can_move_to(board: ChessSet, tile: Tile) -> Bool {
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
