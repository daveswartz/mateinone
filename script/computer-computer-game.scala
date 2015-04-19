import mateinone._
import scala.annotation.tailrec
import TerminalPrinter._

object BoardWithValue {
  def initial: BoardWithValue = {
    val b = Board.initial
    BoardWithValue(b, Evaluation.value(b))
  }
}

case class BoardWithValue(b: Board, value: Int) {
  lazy val leaves: Vector[(MoveBase, BoardWithValue)] =
    b.leaves.map { case (cm, cb) =>
      val v =
        if (!Evaluation.isEndgame(b) && Evaluation.isEndgame(cb)) Evaluation.value(cb)
        else value + Evaluation.deltaValue(b, cm)
      (cm, BoardWithValue(cb, v))
    }
}

var evaluations = 0
def negamax(node: BoardWithValue, depth: Int, color: Int, a: Int, b: Int): Int =
  if (depth == 0 || node.leaves.isEmpty) color * node.value
  else {
    var v = Int.MinValue
    var a0 = a
    evaluations += node.leaves.size
    for (c <- node.leaves.sortBy { case (cm, cb) => -color * cb.value }) {
      v = math.max(v, -negamax(c._2, depth - 1, -color, -b, -a0))
      a0 = math.max(a0, v)
      if (v >= b) return v
    }
    v
  }

var current = BoardWithValue.initial
var start = System.currentTimeMillis
@tailrec def step(depth: Int, color: Int) {
  evaluations = 0
  if (current.b.isCheckmate) println("Checkmate "+current.b.opponent.color.toString+" wins")
  else if (current.b.isStalemate) println("Stalemate")
  else if (current.b.isInsufficientMaterial) println("Insufficient mating material")
  else if (current.b.isThreefoldRepetition) println(current.b.same.color.toString+" claimed draw by threefold repetition")
  else if (current.b.isFiftyMoveRule) println(current.b.same.color.toString+" claimed draw by fifty-move rule")
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
    println("Negamax Score: %d | Leaves: %d | Evaluations: %,d | Elapsed: %.3f | Evaluations/Second: %,.1f".format(bestScore, nOfLeaves, evaluations.intValue, elapsed, evaluations.intValue / elapsed))
    start = end
    step(depth, -color)
  }
}

step(depth = 6, color = 1)
