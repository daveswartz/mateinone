import mateinone._
import scala.annotation.tailrec
import scala.collection.mutable
import TerminalPrinter._

object BoardWithValue {

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
    def value(ofType: Pieces => Set[Square], whiteValues: Map[Square, Int], blackValues: Map[Square, Int]): Int = {
      ofType(b.same).foldLeft(0)(_ + (if (b.turn == White) whiteValues else blackValues)(_)) + ofType(b.opponent).foldLeft(0)(_ + (if (b.turn == White) blackValues else whiteValues)(_))
    }
    value(_.pawns, whitePawn, blackPawn) + value(_.knights, whiteKnight, blackKnight) + value(_.bishops, whiteBishop, blackBishop) +
      value(_.rooks, whiteRook, blackRook) + value(_.queens, whiteQueen, blackQueen) +
      value(_.kings, if (isEndGame) whiteKingEnd else whiteKingMiddle, if (isEndGame) blackKingEnd else blackKingMiddle)
  }

}
case class BoardWithValue(b: Board) {
  lazy val leaves: Vector[(MoveBase, BoardWithValue)] = b.leaves.map { case (k, v) => (k, BoardWithValue(v)) }
  lazy val value: Int = BoardWithValue.boardValue(b)
  lazy val tranpositionKey = (b.same, b.opponent)
}

case class TranspositionValue(depth: Int, a: Int, b: Int, value: Int)
val transpositionTable = mutable.Map.empty[(Pieces, Pieces), TranspositionValue]

var evaluations = 0
def negamax(node: BoardWithValue, depth: Int, color: Int, a: Int, b: Int): Int =
  if (depth == 0 || node.leaves.isEmpty) color * node.value
  else {
    val tOpt = transpositionTable.get(node.tranpositionKey)
    if (tOpt.exists(t => (t.a < t.value && t.value < t.b) || (t.a <= a && b <= t.b) && t.depth >= depth)) tOpt.get.value
    else {
      var v = Int.MinValue
      var a0 = a
      evaluations += node.leaves.size
      for (c <- node.leaves.sortBy { case (cm, cb) => -color * cb.value }) {
        v = math.max(v, -negamax(c._2, depth - 1, -color, -b, -a0))
        a0 = math.max(a0, v)
        if (v >= b) {
          transpositionTable += node.tranpositionKey -> TranspositionValue(depth, a, b, v)
          return v
        }
      }
      transpositionTable += node.tranpositionKey -> TranspositionValue(depth, a, b, v)
      v
    }
  }

var current = BoardWithValue(Board.initial)
var start = System.currentTimeMillis
@tailrec def step(depth: Int, color: Int) {
  evaluations = 0
  if (current.b.isCheckmate) println("Checkmate "+current.b.turn.other.toString+" wins")
  else if (current.b.isStalemate) println("Stalemate")
  else if (current.b.isInsufficientMaterial) println("Insufficient mating material")
  else if (current.b.isThreefoldRepetition) println(current.b.turn.toString+" claimed draw by threefold repetition")
  else if (current.b.isFiftyMoveRule) println(current.b.turn.toString+" claimed draw by fifty-move rule")
  else {
    val (bestMove, bestBoard, bestScore) = {
      var v = -10000
      var a0 = -10000
      var (m0, b0): (MoveBase, BoardWithValue) = (null, null)
      evaluations += current.leaves.size
      for ((cm, cb) <- current.leaves.sortBy { case (_, c) => -color * c.value }) {
        val v0 = math.max(v, -negamax(cb, depth - 1, -color, -10000, -a0))
        a0 = math.max(a0, v0)
        if (v0 > v) { m0 = cm; b0 = cb }
        v = math.max(v, v0)
      }
      (m0, b0, v)
    }
    val nOfLeaves = current.leaves.size
    current = bestBoard
    println(current.b.print(bestMove))
    val end = System.currentTimeMillis
    val elapsed = (end - start)/1000d
    println("Score: %d | Leaves: %d | Evaluations: %d | Elapsed: %.3f | Evaluations/Second: %.3f".format(bestScore, nOfLeaves, evaluations.intValue, elapsed, evaluations.intValue / elapsed))
    start = end
    step(depth, -color)
  }
}

step(depth = 6, color = 1)
