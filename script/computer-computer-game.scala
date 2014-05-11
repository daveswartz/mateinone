import mateinone._
import scala.annotation.tailrec
import TerminalPrinter._

def score(board: Board): Int = {
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

def minimax(node: Board, depth: Int, maximizing: Boolean): Int =
  if (depth == 0 || node.boards.isEmpty) score(node)
  else if (maximizing) node.boards.map(minimax(_, depth - 1, maximizing = false)).max
  else node.boards.map(minimax(_, depth - 1, maximizing = true)).min

def nextMove(node: Board, maximizing: Boolean): MoveBase =
  if (maximizing) node.boards.maxBy(minimax(_, 1, !maximizing)).history.last._1
  else node.boards.minBy(minimax(_, 1, !maximizing)).history.last._1

@tailrec def printAndMove(index: Int, board: Board, maximizing: Boolean, last: Seq[MoveBase]) {
  if (index > 1) println(index/2+". "+last.fold("")(_+" "+_))
  println(board.print)
  if (board.isCheckmate) println("Checkmate "+board.turn.other.toString+" wins")
  else if (board.isStalemate) println("Stalemate")
  else if (board.isInsufficientMaterial) println("Insufficient mating material")
  else if (board.isThreefoldRepetition) println(board.turn.toString+" claimed draw by threefold repetition")
  else if (board.isFiftyMoveRule) println(board.turn.toString+" claimed draw by fifty-move rule")
  else {
    val next = nextMove(board, maximizing)
    printAndMove(index + 1, board.move(next).get, !maximizing, if (last.size == 2) Seq(next) else last :+ next)
  }
}
printAndMove(1, Board.initial, maximizing = true, Seq())
