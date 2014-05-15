import mateinone._
import scala.annotation.tailrec
import TerminalPrinter._

def score(piece: Piece): Int =
  piece.`type` match { case Pawn => 100; case Knight => 350; case Bishop => 350; case Rook => 525; case Queen => 1000; case King => 0 }

def score(board: Board): Int =
  board.pieces.map(p => (if (board.turn == p.side) 1 else -1) * score(p)).sum

def score(node: Board, depth: Int): Int =
  if (depth == 0 || node.boards.isEmpty) score(node)
  else node.boards.map(-score(_, depth - 1)).max

def nextMove(node: Board, depth: Int): MoveBase =
  node.boards.maxBy(-score(_, depth - 1)).history.last._1

@tailrec def step(board: Board) {
  println(board.print)
  if (board.isCheckmate) println("Checkmate "+board.turn.other.toString+" wins")
  else if (board.isStalemate) println("Stalemate")
  else if (board.isInsufficientMaterial) println("Insufficient mating material")
  else if (board.isThreefoldRepetition) println(board.turn.toString+" claimed draw by threefold repetition")
  else if (board.isFiftyMoveRule) println(board.turn.toString+" claimed draw by fifty-move rule")
  else step(board.move(nextMove(board, 2)).get)
}

step(Board.initial)
