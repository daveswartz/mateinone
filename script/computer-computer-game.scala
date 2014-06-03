import java.util.concurrent.atomic.AtomicInteger
import mateinone._
import scala.annotation.tailrec
import TerminalPrinter._

object PieceSquareTables {

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

  def squareValue(side: Side, `type`: PieceType, square: Square, isEndGame: Boolean = false): Int =
    squareValues(`type`, isEndGame)(if (side == White) 7 - square.rank.n else square.rank.n)(square.file.n)

}
import PieceSquareTables._

def pieceTypeValue(`type`: PieceType): Int =
  `type` match { case Pawn => 100; case Knight => 320; case Bishop => 330; case Rook => 500; case Queen => 900; case King => 0 }

implicit def boardWithValue(b: Board) = new {
  val value: Int = {
    val isEndGame = b.pieces.count(_.`type` == Queen) == 0
    def value(piece: Piece): Int = squareValue(piece.side,  piece.`type`, piece.square, isEndGame) + pieceTypeValue(piece.`type`)
    b.pieces.toVector.map(p => (if (White == p.side) 1 else -1) * value(p)).sum
  }
}

var evaluations = new AtomicInteger(0)
def negamax(node: Board, depth: Int, color: Int): Int =
  if (depth == 0 || node.boards.isEmpty) { evaluations.getAndIncrement; color * node.value }
  else node.boards.par.map(-negamax(_, depth - 1, -color)).max

var board = Board.initial
var start = System.currentTimeMillis
@tailrec def step(depth: Int, color: Int) {
  evaluations = new AtomicInteger(0)
  if (board.isCheckmate) println("Checkmate "+board.turn.other.toString+" wins")
  else if (board.isStalemate) println("Stalemate")
  else if (board.isInsufficientMaterial) println("Insufficient mating material")
  else if (board.isThreefoldRepetition) println(board.turn.toString+" claimed draw by threefold repetition")
  else if (board.isFiftyMoveRule) println(board.turn.toString+" claimed draw by fifty-move rule")
  else {
    val (bestMove, bestBoard) = board.leaves.minBy { case (_, b) => negamax(b, depth - 1, -color) }
    val nOfLeaves = board.leaves.size
    board = bestBoard
    println(board.print(Some(bestMove)))
    val end = System.currentTimeMillis
    val elapsed = (end - start)/1000d
    println("Score: %d | Leaves: %d | Evaluations: %d | Elapsed: %.3f | Evaluations/Second: %.3f".format(board.value, nOfLeaves, evaluations.intValue, elapsed, evaluations.intValue / elapsed))
    start = end
    step(depth, -color)
  }
}

println(board.print(None))
step(depth = 4, color = 1)
