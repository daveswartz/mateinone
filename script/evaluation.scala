import mateinone._
import mateinone.Piece
import TerminalPrinter._
import MovePrinter._

def score(board: Board) = {
  def value(pieces: Vector[Piece]) = pieces.map(_.`type`).map {
    case Pawn => 100
    case Knight => 350
    case Bishop => 350
    case Rook => 525
    case Queen => 1000
    case King => 0
  }.sum
  board.pieces.partition(_.side == White) match { case (w, b) => value(w) - value(b) }
}

def bfs(board: Board, limit: Int): Board =
    if (limit == 0 || board.moves.isEmpty) board
    else board.moves.par.map(m => bfs(board.move(m).get, limit - 1)).maxBy(b => score(b))

println("Starting...")
val start = System.nanoTime()
val max = bfs(Board.initial, 4)
val elapsed = (System.nanoTime() - start) / 1e9
println("Elapsed (s): "+elapsed)
println("Score: "+ score(max))
println("Moves: "+max.moveHistory.map(_.print).reduce(_+", "+_))
println(max.print)
