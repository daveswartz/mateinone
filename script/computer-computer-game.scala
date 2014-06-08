import mateinone._
import scala.annotation.tailrec
import scala.collection.mutable
import TerminalPrinter._

object BoardValue {

  private def groupByRank(v: Vector[Int]): Vector[Vector[Int]] = v.grouped(8).toVector

  private val pawn: Vector[Vector[Int]] = groupByRank(Vector(
      0,   0,   0,   0,   0,   0,   0,   0,
     50,  50,  50,  50,  50,  50,  50,  50,
     10,  10,  20,  30,  30,  20,  10,  10,
      5,   5,  10,  25,  25,  10,   5,   5,
      0,   0,   0,  20,  20,   0,   0,   0,
      5,  -5, -10,   0,   0, -10,  -5,   5,
      5,  10,  10, -20, -20,  10,  10,   5,
      0,   0,   0,   0,   0,   0,   0,   0
  ))

  private val knight: Vector[Vector[Int]] = groupByRank(Vector(
    -50, -40, -30, -30, -30, -30, -40, -50,
    -40, -20,   0,   0,   0,   0, -20, -40,
    -30,   0,  10,  15,  15,  10,   0, -30,
    -30,   5,  15,  20,  20,  15,   5, -30,
    -30,   0,  15,  20,  20,  15,   0, -30,
    -30,   5,  10,  15,  15,  10,   5, -30,
    -40, -20,   0,   5,   5,   0, -20, -40,
    -50, -40, -30, -30, -30, -30, -40, -50
  ))

  private val bishop: Vector[Vector[Int]] = groupByRank(Vector(
    -20, -10, -10, -10, -10, -10, -10, -20,
    -10,   0,   0,   0,   0,   0,   0, -10,
    -10,   0,   5,  10,  10,   5,   0, -10,
    -10,   5,   5,  10,  10,   5,   5, -10,
    -10,   0,  10,  10,  10,  10,   0, -10,
    -10,  10,  10,  10,  10,  10,  10, -10,
    -10,   5,   0,   0,   0,   0,   5, -10,
    -20, -10, -10, -10, -10, -10, -10, -20
  ))

  private val rook: Vector[Vector[Int]] = groupByRank(Vector(
      0,   0,   0,   0,   0,   0,   0,   0,
      5,  10,  10,  10,  10,  10,  10,   5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
      0,   0,   0,   5,   5,   0,   0,   0
  ))

  private val queen: Vector[Vector[Int]] = groupByRank(Vector(
    -20, -10, -10,  -5,  -5, -10, -10, -20,
    -10,   0,   0,   0,   0,   0,   0, -10,
    -10,   0,   5,   5,   5,   5,   0, -10,
     -5,   0,   5,   5,   5,   5,   0,  -5,
      0,   0,   5,   5,   5,   5,   0,  -5,
    -10,   5,   5,   5,   5,   5,   0, -10,
    -10,   0,   5,   0,   0,   0,   0, -10,
    -20, -10, -10,  -5,  -5, -10, -10, -20
  ))

  private val kingMiddleGame: Vector[Vector[Int]] = groupByRank(Vector(
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -20, -30, -30, -40, -40, -30, -30, -20,
    -10, -20, -20, -20, -20, -20, -20, -10,
     20,  20,   0,   0,   0,   0,  20,  20,
     20,  30,  10,   0,   0,  10,  30,  20
  ))

  private val kingEndGame: Vector[Vector[Int]] = groupByRank(Vector(
    -50, -40, -30, -20, -20, -30, -40, -50,
    -30, -20, -10,   0,   0, -10, -20, -30,
    -30, -10,  20,  30,  30,  20, -10, -30,
    -30, -10,  30,  40,  40,  30, -10, -30,
    -30, -10,  30,  40,  40,  30, -10, -30,
    -30, -10,  20,  30,  30,  20, -10, -30,
    -30, -30,   0,   0,   0,   0, -30, -30,
    -50, -30, -30, -30, -30, -30, -30, -50
  ))

  private def squareValues(`type`: PieceType, isEndGame: Boolean): Vector[Vector[Int]] = `type` match {
    case Pawn => pawn; case Knight => knight; case Bishop => bishop; case Rook => rook; case Queen => queen;
    case King => if (isEndGame) kingEndGame else kingMiddleGame }

  private def squareValue(side: Side, `type`: PieceType, square: Square, isEndGame: Boolean = false): Int =
    squareValues(`type`, isEndGame)(if (side == White) 7 - square.rank else square.rank)(square.file)

  private def pieceTypeValue(t: PieceType): Int =
    t match { case Pawn => 100; case Knight => 320; case Bishop => 330; case Rook => 500; case Queen => 900; case King => 0 }

  private def boardValue(b: Board): Int = {
    val isEndGame = b.pieces.count(_.`type` == Queen) == 0
    var v = 0
    for (p <- b.pieces)
      v += (if (p.side == White) 1 else -1) * (squareValue(p.side,  p.`type`, p.square, isEndGame) + pieceTypeValue(p.`type`))
    v
  }

  implicit def boardWithValue(b: Board) = new { val value: Int = boardValue(b) }

}
import BoardValue._

case class TranspositionValue(depth: Int, a: Int, b: Int, value: Int)
val transpositionTable = mutable.Map.empty[Vector[Piece], TranspositionValue]

var evaluations = 0
def negamax(node: Board, depth: Int, color: Int, a: Int, b: Int): Int =
  if (depth == 0 || node.boards.isEmpty) color * node.value
  else {
    val tOpt = transpositionTable.get(node.pieces)
    if (tOpt.exists(t => (t.a < t.value && t.value < t.b) || (t.a <= a && b <= t.b) && t.depth >= depth)) tOpt.get.value
    else {
      var v = Int.MinValue
      var a0 = a
      evaluations += node.boards.size
      for (c <- node.boards.sortBy(c => -color * c.value)) {
        v = math.max(v, -negamax(c, depth - 1, -color, -b, -a0))
        a0 = math.max(a0, v)
        if (v >= b) {
          transpositionTable += node.pieces -> TranspositionValue(depth, a, b, v)
          return v
        }
      }
      transpositionTable += node.pieces -> TranspositionValue(depth, a, b, v)
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
    println(current.print(Some(bestMove)))
    val end = System.currentTimeMillis
    val elapsed = (end - start)/1000d
    println("Score: %d | Leaves: %d | Evaluations: %d | Elapsed: %.3f | Evaluations/Second: %.3f".format(bestScore, nOfLeaves, evaluations.intValue, elapsed, evaluations.intValue / elapsed))
    start = end
    step(depth, -color)
  }
}

step(depth = 6, color = 1)
