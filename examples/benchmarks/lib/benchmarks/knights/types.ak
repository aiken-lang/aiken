pub type Tile =
  (Int, Int)

pub type ChessSet {
  size: Int,
  move_number: Int,
  visited: List<Tile>,
  start: Option<Tile>,
}

pub type Solution =
  List<(Int, ChessSet)>
