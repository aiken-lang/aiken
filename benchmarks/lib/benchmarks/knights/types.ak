pub type Tile =
  (Int, Int)

pub type ChessSet {
  size: Int,
  move_number: Int,
  start: Option<Tile>,
  visited: List<Tile>,
}

pub type Solution =
  List<(Int, ChessSet)>
