import mateinone._
import scala.annotation.tailrec
import scala.collection.mutable
import TerminalPrinter._

object BoardWithValue {
  def initial: BoardWithValue = {
    val b = Board.initial
    BoardWithValue(b, Evaluation.value(b))
  }
}

case class BoardWithValue(b: Board, value: Int) {
  lazy val leaves: Vector[(MoveBase, BoardWithValue)] =
    b.leaves.map { case (cm, cb) => (cm, BoardWithValue(cb, value + Evaluation.deltaValue(b, cm))) } // TODO reevaluate on end game in Evaluation
//  lazy val tranpositionKey: TranspositionKey = (b.same, b.opponent)
}

//type TranspositionKey = (Pieces, Pieces)
//case class TranspositionValue(depth: Int, a: Int, b: Int, value: Int)
//class TranspositionTable(private val map: mutable.Map[TranspositionKey, TranspositionValue] = mutable.Map.empty[TranspositionKey, TranspositionValue]) {
//  def get(k: TranspositionKey): Option[TranspositionValue] = map.get(k)
//  def put(k: TranspositionKey, v: TranspositionValue) = {
//    if (map.size > 1000) map.remove(map.head._1)
//    map += k -> v
//  }
//}
//val transpositionTable = new TranspositionTable()

var evaluations = 0
def negamax(node: BoardWithValue, depth: Int, color: Int, a: Int, b: Int): Int =
  if (depth == 0 || node.leaves.isEmpty) color * node.value
  else {
//    val tOpt = transpositionTable.get(node.tranpositionKey)
//    if (tOpt.exists(t => (t.a < t.value && t.value < t.b) || (t.a <= a && b <= t.b) && t.depth >= depth)) tOpt.get.value
//    else {
      var v = Int.MinValue
      var a0 = a
      evaluations += node.leaves.size
      for (c <- node.leaves.sortBy { case (cm, cb) => -color * cb.value }) {
        v = math.max(v, -negamax(c._2, depth - 1, -color, -b, -a0))
        a0 = math.max(a0, v)
        if (v >= b) {
//          transpositionTable.put(node.tranpositionKey, TranspositionValue(depth, a, b, v))
          return v
        }
      }
//      transpositionTable.put(node.tranpositionKey, TranspositionValue(depth, a, b, v))
      v
//    }
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
