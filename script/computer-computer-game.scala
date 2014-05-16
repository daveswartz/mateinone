import mateinone._
import scala.annotation.tailrec
import TerminalPrinter._

// Evaluation http://chessprogramming.wikispaces.com/Evaluation
// Piece-Square Tables http://chessprogramming.wikispaces.com/Piece-Square+Tables
// Simplified evaluation function http://chessprogramming.wikispaces.com/Simplified+evaluation+function

val pawnSquareValues: Vector[Vector[Int]] =
  Vector(  0,  0,  0,  0,  0,  0,  0,  0,
          50, 50, 50, 50, 50, 50, 50, 50,
          10, 10, 20, 30, 30, 20, 10, 10,
           5,  5, 10, 25, 25, 10,  5,  5,
           0,  0,  0, 20, 20,  0,  0,  0,
           5, -5,-10,  0,  0,-10, -5,  5,
           5, 10, 10,-20,-20, 10, 10,  5,
           0,  0,  0,  0,  0,  0,  0,  0).grouped(8).toVector

val knightSquareValues: Vector[Vector[Int]] =
  Vector(-50,-40,-30,-30,-30,-30,-40,-50,
         -40,-20,  0,  0,  0,  0,-20,-40,
         -30,  0, 10, 15, 15, 10,  0,-30,
         -30,  5, 15, 20, 20, 15,  5,-30,
         -30,  0, 15, 20, 20, 15,  0,-30,
         -30,  5, 10, 15, 15, 10,  5,-30,
         -40,-20,  0,  5,  5,  0,-20,-40,
         -50,-40,-30,-30,-30,-30,-40,-50).grouped(8).toVector

val bishopSquareValues: Vector[Vector[Int]] =
  Vector(-20,-10,-10,-10,-10,-10,-10,-20,
         -10,  0,  0,  0,  0,  0,  0,-10,
         -10,  0,  5, 10, 10,  5,  0,-10,
         -10,  5,  5, 10, 10,  5,  5,-10,
         -10,  0, 10, 10, 10, 10,  0,-10,
         -10, 10, 10, 10, 10, 10, 10,-10,
         -10,  5,  0,  0,  0,  0,  5,-10,
         -20,-10,-10,-10,-10,-10,-10,-20).grouped(8).toVector

val rookSquareValues: Vector[Vector[Int]] =
  Vector(  0,  0,  0,  0,  0,  0,  0,  0,
           5, 10, 10, 10, 10, 10, 10,  5,
          -5,  0,  0,  0,  0,  0,  0, -5,
          -5,  0,  0,  0,  0,  0,  0, -5,
          -5,  0,  0,  0,  0,  0,  0, -5,
          -5,  0,  0,  0,  0,  0,  0, -5,
          -5,  0,  0,  0,  0,  0,  0, -5,
           0,  0,  0,  5,  5,  0,  0,  0).grouped(8).toVector

val queenSquareValues: Vector[Vector[Int]] =
  Vector(-20,-10,-10, -5, -5,-10,-10,-20,
         -10,  0,  0,  0,  0,  0,  0,-10,
         -10,  0,  5,  5,  5,  5,  0,-10,
          -5,  0,  5,  5,  5,  5,  0, -5,
           0,  0,  5,  5,  5,  5,  0, -5,
         -10,  5,  5,  5,  5,  5,  0,-10,
         -10,  0,  5,  0,  0,  0,  0,-10,
         -20,-10,-10, -5, -5,-10,-10,-20).grouped(8).toVector

val kingMiddleGameSquareValues: Vector[Vector[Int]] =
  Vector(-30,-40,-40,-50,-50,-40,-40,-30,
         -30,-40,-40,-50,-50,-40,-40,-30,
         -30,-40,-40,-50,-50,-40,-40,-30,
         -30,-40,-40,-50,-50,-40,-40,-30,
         -20,-30,-30,-40,-40,-30,-30,-20,
         -10,-20,-20,-20,-20,-20,-20,-10,
          20, 20,  0,  0,  0,  0, 20, 20,
          20, 30, 10,  0,  0, 10, 30, 20).grouped(8).toVector

val kingEndGameSquareValues: Vector[Vector[Int]] =
  Vector(-50,-40,-30,-20,-20,-30,-40,-50,
         -30,-20,-10,  0,  0,-10,-20,-30,
         -30,-10, 20, 30, 30, 20,-10,-30,
         -30,-10, 30, 40, 40, 30,-10,-30,
         -30,-10, 30, 40, 40, 30,-10,-30,
         -30,-10, 20, 30, 30, 20,-10,-30,
         -30,-30,  0,  0,  0,  0,-30,-30,
         -50,-30,-30,-30,-30,-30,-30,-50).grouped(8).toVector

def score(side: Side, square: Square, pieceValue: Int, squareValues: Vector[Vector[Int]]): Int =
  squareValues(if (side == White) 7 - square.rank.n else square.rank.n)(square.file.n) + pieceValue

def score(piece: Piece, endGame: Boolean): Int =
  piece match {
    case Piece(side, Pawn,   square, _) => score(side, square, 100, pawnSquareValues)
    case Piece(side, Knight, square, _) => score(side, square, 320, knightSquareValues)
    case Piece(side, Bishop, square, _) => score(side, square, 330, bishopSquareValues)
    case Piece(side, Rook,   square, _) => score(side, square, 500, rookSquareValues)
    case Piece(side, Queen,  square, _) => score(side, square, 900, queenSquareValues)
    case Piece(side, King,   square, _) => score(side, square, 0,   if (endGame) kingEndGameSquareValues else kingMiddleGameSquareValues)
  }

def isEndGame(b: Board): Boolean = b.pieces.count(_.`type` == Queen) == 0

def score(board: Board): Int =
  board.pieces.map(p => (if (board.turn == p.side) 1 else -1) * score(p, isEndGame(board))).sum

def score(node: Board, depth: Int): Int =
  if (depth == 0 || node.boards.isEmpty) score(node)
  else node.boards.map(-score(_, depth - 1)).max

def nextMove(node: Board, depth: Int): MoveBase =
  node.boards.par.maxBy(-score(_, depth - 1)).history.last._1

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
