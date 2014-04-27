import java.util.concurrent._
import collection.JavaConverters._
import mateinone._
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

val evaluations = new ConcurrentHashMap[Vector[Piece], Int].asScala

def bfs(board: Board, limit: Int): Board = {
  evaluations.getOrElseUpdate(board.pieces, score(board))
  if (limit > 0)
    if (board.moves.isEmpty) board
    else board.moves.par.map(m => bfs(board.move(m).get, limit - 1)).maxBy(b => evaluations(b.pieces))
  else board
}

val max = bfs(Board.initial, 5)
println("Evaluations: "+evaluations.size)
println("Score: "+evaluations(max.pieces))
println("Moves: "+max.moveHistory.map(_.print).reduce(_+", "+_))
println(max.print)
