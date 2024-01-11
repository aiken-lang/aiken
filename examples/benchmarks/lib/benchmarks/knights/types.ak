pub type Tile =
  (Int, Int)

pub type ChessSet {
  move_number: Int,
  visited: List<Tile>,
  size: Int,
  start: Option<Tile>,
}

pub type Solution =
  List<(Int, ChessSet)>
