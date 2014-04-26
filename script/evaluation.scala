import mateinone._
import TerminalPrinter._
import MovePrinter._

def value(pieces: Vector[Piece]) = pieces.map(_.`type`).map {
  case Pawn => 100
  case Knight => 350
  case Bishop => 350
  case Rook => 525
  case Queen => 1000
  case King => 0
}.sum

def evaluate(board: Board): Int =
  board.pieces.partition(_.side == board.turn) match { case (cur, opp) => value(cur) - value(opp) }

def greedyBfs(board: Board, limit: Int): Board =
  if (limit > 0)
    board.moves.map { m =>
      val after = board.move(m).get
      if (-evaluate(after) < evaluate(board)) board
      else greedyBfs(after, limit -1)
    }.maxBy(evaluate)
  else board

val max = greedyBfs(Board.initial, 4)
println("Score: "+evaluate(max))
println("Moves: "+max.moveHistory.map(_.print).reduce(_+", "+_))
println(max.print)
