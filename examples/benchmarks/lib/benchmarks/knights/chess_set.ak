use aiken/builtin
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
  // record update
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

// {-# INLINABLE lastPiece #-}
// lastPiece :: ChessSet -> Tile
// lastPiece (Board _ _ _ (t:_)) = t
// lastPiece _                   = Tx.error ()

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
  not_in(board.visited, tile)
}

pub fn not_in(tiles: List<a>, tile: a) -> Bool {
  !list.has(tiles, tile)
}
// {-# INLINABLE positionPiece #-}
// positionPiece :: Integer -> ChessSet -> Tile
// positionPiece x (Board _ n _ ts) = ts Tx.!! (n - x)

// {-# INLINABLE pieceAtTile #-}
// pieceAtTile :: Tile -> ChessSet -> Integer
// pieceAtTile x0 (Board _ _ _ ts)
//    = findPiece x0 ts
//      where
//         findPiece _ [] = Tx.error ()
//         findPiece x (y:xs)
//            | x == y    = 1 + Tx.length xs
//            | otherwise = findPiece x xs

// -- % Everything below here is only needed for printing boards.
// -- % This is useful for debugging.

// instance Haskell.Show ChessSet where
//    showsPrec _ (Board sze n _ ts)
//       = Haskell.showString (printBoard sze sortedTrail 1)
//         where sortedTrail = quickSort (assignMoveNo ts sze n)

// assignMoveNo :: [Tile] -> Integer -> Integer -> [Tile]
// assignMoveNo [] _ _
//    = []
// assignMoveNo ((x,y):t) size z
//    = (((y-1)*size)+x,z):assignMoveNo t size (z-1)

// printBoard :: Integer -> [Tile] -> Integer -> Haskell.String
// printBoard s [] n
//    | (n  > (s*s))   = ""
//    | ((n `Haskell.mod` s) /=0)= "*"++(spaces (s*s) 1) ++(printBoard s [] (n+1))
//    | ((n `Haskell.mod` s) ==0)= "*\n"                 ++(printBoard s [] (n+1))
// printBoard s trail@((i,j):xs) n
//    | (i==n) &&
//      ((n `Haskell.mod` s) ==0) = (Haskell.show j)++"\n"++(printBoard s xs (n+1))
//    | (i==n) &&
//      ((n `Haskell.mod` s) /=0)= (Haskell.show j)++(spaces (s*s) j)++(printBoard s xs    (n+1))
//    | ((n `Haskell.mod` s) /=0)= "*"     ++(spaces (s*s) 1)++(printBoard s trail (n+1))
//    | ((n `Haskell.mod` s) ==0)= "*\n"                     ++(printBoard s trail (n+1))
// printBoard _ _ _ = "?"

// spaces :: Integer -> Integer -> Haskell.String
// spaces s y =
//     take' ((logTen s) - (logTen y) + 1) [' ',' '..]
//         where
//           logTen :: Integer -> Integer
//           logTen 0 = 0
//           logTen x = 1 + logTen (x `Haskell.div` 10)
