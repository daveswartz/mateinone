import mateinone._
import scala.annotation.tailrec
import scala.collection.mutable
import TerminalPrinter._

object BoardValue {

  private val whiteSquares = Square.Squares.reverse.flatten
  private val blackSquares = Square.Squares.flatten
  private def sides(pieceSquareTable: Vector[Int], pieceValue: Int) =
    (blackSquares.zip(pieceSquareTable.map(_ + pieceValue)).toMap, whiteSquares.zip(pieceSquareTable.map(-_ - pieceValue)).toMap)

  private val pawnSquareTable = Vector(
      0,   0,   0,   0,   0,   0,   0,   0,
     50,  50,  50,  50,  50,  50,  50,  50,
     10,  10,  20,  30,  30,  20,  10,  10,
      5,   5,  10,  25,  25,  10,   5,   5,
      0,   0,   0,  20,  20,   0,   0,   0,
      5,  -5, -10,   0,   0, -10,  -5,   5,
      5,  10,  10, -20, -20,  10,  10,   5,
      0,   0,   0,   0,   0,   0,   0,   0
  )

  private val knightSquareTable = Vector(
    -50, -40, -30, -30, -30, -30, -40, -50,
    -40, -20,   0,   0,   0,   0, -20, -40,
    -30,   0,  10,  15,  15,  10,   0, -30,
    -30,   5,  15,  20,  20,  15,   5, -30,
    -30,   0,  15,  20,  20,  15,   0, -30,
    -30,   5,  10,  15,  15,  10,   5, -30,
    -40, -20,   0,   5,   5,   0, -20, -40,
    -50, -40, -30, -30, -30, -30, -40, -50
  )

  private val bishopSquareTable = Vector(
    -20, -10, -10, -10, -10, -10, -10, -20,
    -10,   0,   0,   0,   0,   0,   0, -10,
    -10,   0,   5,  10,  10,   5,   0, -10,
    -10,   5,   5,  10,  10,   5,   5, -10,
    -10,   0,  10,  10,  10,  10,   0, -10,
    -10,  10,  10,  10,  10,  10,  10, -10,
    -10,   5,   0,   0,   0,   0,   5, -10,
    -20, -10, -10, -10, -10, -10, -10, -20
  )

  private val rookSquareTable = Vector(
      0,   0,   0,   0,   0,   0,   0,   0,
      5,  10,  10,  10,  10,  10,  10,   5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
      0,   0,   0,   5,   5,   0,   0,   0
  )

  private val queenSquareTable = Vector(
    -20, -10, -10,  -5,  -5, -10, -10, -20,
    -10,   0,   0,   0,   0,   0,   0, -10,
    -10,   0,   5,   5,   5,   5,   0, -10,
     -5,   0,   5,   5,   5,   5,   0,  -5,
      0,   0,   5,   5,   5,   5,   0,  -5,
    -10,   5,   5,   5,   5,   5,   0, -10,
    -10,   0,   5,   0,   0,   0,   0, -10,
    -20, -10, -10,  -5,  -5, -10, -10, -20
  )

  private val kingMiddleSquareTable = Vector(
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -20, -30, -30, -40, -40, -30, -30, -20,
    -10, -20, -20, -20, -20, -20, -20, -10,
     20,  20,   0,   0,   0,   0,  20,  20,
     20,  30,  10,   0,   0,  10,  30,  20
  )

  private val kingEndSquareTable = Vector(
    -50, -40, -30, -20, -20, -30, -40, -50,
    -30, -20, -10,   0,   0, -10, -20, -30,
    -30, -10,  20,  30,  30,  20, -10, -30,
    -30, -10,  30,  40,  40,  30, -10, -30,
    -30, -10,  30,  40,  40,  30, -10, -30,
    -30, -10,  20,  30,  30,  20, -10, -30,
    -30, -30,   0,   0,   0,   0, -30, -30,
    -50, -30, -30, -30, -30, -30, -30, -50
  )

  private val (whitePawn, blackPawn) = sides(pawnSquareTable, 100)
  private val (whiteKnight, blackKnight) = sides(knightSquareTable, 320)
  private val (whiteBishop, blackBishop) = sides(bishopSquareTable, 330)
  private val (whiteRook, blackRook) = sides(rookSquareTable, 500)
  private val (whiteQueen, blackQueen) = sides(queenSquareTable, 900)
  private val (whiteKingMiddle, blackKingMiddle) = sides(kingMiddleSquareTable, 0)
  private val (whiteKingEnd, blackKingEnd) = sides(kingEndSquareTable, 0)

  private def boardValue(b: Board): Int = {
    val isEndGame = b.same.queens.isEmpty || b.opponent.queens.isEmpty
    if (b.turn == White) {
      b.same.pawns.foldLeft(0)(_ + whitePawn(_)) + b.opponent.pawns.foldLeft(0)(_ + blackPawn(_)) +
        b.same.knights.foldLeft(0)(_ + whiteKnight(_)) + b.opponent.knights.foldLeft(0)(_ + blackKnight(_)) +
        b.same.bishops.foldLeft(0)(_ + whiteBishop(_)) + b.opponent.bishops.foldLeft(0)(_ + blackBishop(_)) +
        b.same.rooks.foldLeft(0)(_ + whiteRook(_)) + b.opponent.rooks.foldLeft(0)(_ + blackRook(_)) +
        b.same.queens.foldLeft(0)(_ + whiteQueen(_)) + b.opponent.queens.foldLeft(0)(_ + blackQueen(_)) +
        (if (isEndGame) b.same.kings.foldLeft(0)(_ + whiteKingMiddle(_)) + b.opponent.kings.foldLeft(0)(_ + blackKingMiddle(_))
        else b.same.kings.foldLeft(0)(_ + whiteKingEnd(_)) + b.opponent.kings.foldLeft(0)(_ + blackKingEnd(_)))
    } else {
      b.same.pawns.foldLeft(0)(_ + blackPawn(_)) + b.opponent.pawns.foldLeft(0)(_ + whitePawn(_)) +
        b.same.knights.foldLeft(0)(_ + blackKnight(_)) + b.opponent.knights.foldLeft(0)(_ + whiteKnight(_)) +
        b.same.bishops.foldLeft(0)(_ + blackBishop(_)) + b.opponent.bishops.foldLeft(0)(_ + whiteBishop(_)) +
        b.same.rooks.foldLeft(0)(_ + blackRook(_)) + b.opponent.rooks.foldLeft(0)(_ + whiteRook(_)) +
        b.same.queens.foldLeft(0)(_ + blackQueen(_)) + b.opponent.queens.foldLeft(0)(_ + whiteQueen(_)) +
        (if (isEndGame) b.same.kings.foldLeft(0)(_ + blackKingMiddle(_)) + b.opponent.kings.foldLeft(0)(_ + whiteKingMiddle(_))
        else b.same.kings.foldLeft(0)(_ + blackKingEnd(_)) + b.opponent.kings.foldLeft(0)(_ + whiteKingEnd(_)))
    }
  }

  implicit def boardWithValue(b: Board) = new { def value: Int = boardValue(b) }

}
import BoardValue._

case class TranspositionValue(depth: Int, a: Int, b: Int, value: Int)
val transpositionTable = mutable.Map.empty[(Pieces, Pieces), TranspositionValue]

var evaluations = 0
def negamax(node: Board, depth: Int, color: Int, a: Int, b: Int): Int =
  if (depth == 0 || node.boards.isEmpty) color * node.value
  else {
    val tOpt = transpositionTable.get((node.same, node.opponent))
    if (tOpt.exists(t => (t.a < t.value && t.value < t.b) || (t.a <= a && b <= t.b) && t.depth >= depth)) tOpt.get.value
    else {
      var v = Int.MinValue
      var a0 = a
      evaluations += node.boards.size
      for (c <- node.boards.sortBy(c => -color * c.value)) {
        v = math.max(v, -negamax(c, depth - 1, -color, -b, -a0))
        a0 = math.max(a0, v)
        if (v >= b) {
          transpositionTable += (node.same, node.opponent) -> TranspositionValue(depth, a, b, v)
          return v
        }
      }
      transpositionTable += (node.same, node.opponent) -> TranspositionValue(depth, a, b, v)
      v
    }
  }

var current = Board.initial
var start = System.currentTimeMillis
@tailrec def step(depth: Int, color: Int) {
  evaluations = 0
  if (current.isCheckmate) println("Checkmate "+current.turn.other.toString+" wins")
  else if (current.isStalemate) println("Stalemate")
  else if (current.isInsufficientMaterial) println("Insufficient mating material")
  else if (current.isThreefoldRepetition) println(current.turn.toString+" claimed draw by threefold repetition")
  else if (current.isFiftyMoveRule) println(current.turn.toString+" claimed draw by fifty-move rule")
  else {
    val (bestMove, bestBoard, bestScore) = {
      var v = -10000
      var a0 = -10000
      var (m0, b0): (MoveBase, Board) = (null, null)
      evaluations += current.leaves.size
      for ((m, b) <- current.leaves.sortBy { case (_, c) => -color * c.value }) {
        val v0 = math.max(v, -negamax(b, depth - 1, -color, -10000, -a0))
        a0 = math.max(a0, v0)
        if (v0 > v) { m0 = m; b0 = b }
        v = math.max(v, v0)
      }
      (m0, b0, v)
    }
    val nOfLeaves = current.leaves.size
    current = bestBoard
    println(current.print(bestMove))
    val end = System.currentTimeMillis
    val elapsed = (end - start)/1000d
    println("Score: %d | Leaves: %d | Evaluations: %d | Elapsed: %.3f | Evaluations/Second: %.3f".format(bestScore, nOfLeaves, evaluations.intValue, elapsed, evaluations.intValue / elapsed))
    start = end
    step(depth, -color)
  }
}

step(depth = 6, color = 1)
