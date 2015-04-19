import mateinone._
import TerminalPrinter._

def alphaBetaMax(board: BoardWithEvaluator, alpha: Int, beta: Int, depth: Int): (Int, List[MoveBase]) =
  if (depth == 0) (board.evaluation, Nil)
  else {
    var (maxAlpha, bestMoves) = (alpha, List.empty[MoveBase])
    for (leaf <- board.leaves) {
      val (score, moves) = alphaBetaMin(leaf._2, maxAlpha, beta, depth - 1)
      if (score >= beta) return (beta, leaf._1 :: moves) // fail hard beta-cutoff
      if (score > maxAlpha) { maxAlpha = score; bestMoves = leaf._1 :: moves } // alpha acts like max in MiniMax
    }
    (maxAlpha, bestMoves)
  }

def alphaBetaMin(board: BoardWithEvaluator, alpha: Int, beta: Int, depth: Int): (Int, List[MoveBase]) =
  if (depth == 0) (board.evaluation, Nil)
  else {
    var (minBeta, bestMoves) = (beta, List.empty[MoveBase])
    for (leaf <- board.leaves) {
      val (score, moves) = alphaBetaMax(leaf._2, alpha, minBeta, depth - 1)
      if (score <= alpha) return (alpha, leaf._1 :: moves) // fail hard alpha-cutoff
      if (score < minBeta) { minBeta = score; bestMoves = leaf._1 :: moves } // beta acts like min in MiniMax
    }
    (minBeta, bestMoves)
  }

def next(b: BoardWithEvaluator, depth: Int): (Int, List[MoveBase]) =
  if (b.b.same.color == White)
    alphaBetaMax(b, Int.MinValue, Int.MaxValue, depth)
  else
    alphaBetaMin(b, Int.MinValue, Int.MaxValue, depth)

def step(b: Board, depth: Int, n: Int): Unit =
  if (b.isCheckmate) println(s"Checkmate ${b.opponent.color.toString} wins")
  else if (b.isStalemate) println("Stalemate")
  else if (b.isInsufficientMaterial) println("Insufficient mating material")
  else if (b.isThreefoldRepetition) println(s"${b.same.color.toString} claimed draw by threefold repetition")
  else if (b.isFiftyMoveRule) println(s"${b.same.color.toString} claimed draw by fifty-move rule")
  else {
    val (ns, nm :: rm) = next(BoardWithEvaluator(b), depth)
    val nb = b.move(nm).get
    println(nb.print(nm))
    println(s"${n / 2 + 1}. $nm | score: $ns | moves: ${rm.mkString(", ")}")
    step(nb, depth, n + 1)
  }

step(Board.initial, 5, 0)

/*
var evaluations = 0
def negamax(node: BoardWithScore, depth: Int, color: Int, a: Int, b: Int): Int =
  if (depth == 0 || node.leaves.isEmpty) color * node.score
  else {
    var v = Int.MinValue
    var a0 = a
    evaluations += node.leaves.size
    for (c <- node.leaves.sortBy { case (cm, cb) => -color * cb.score }) {
      v = math.max(v, -negamax(c._2, depth - 1, -color, -b, -a0))
      a0 = math.max(a0, v)
      if (v >= b) return v
    }
    v
  }

var current = BoardWithScore(Board.initial)
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
      var (m0, b0): (MoveBase, BoardWithScore) = (null, null)
      evaluations += current.leaves.size
      for ((cm, cb) <- current.leaves.sortBy { case (_, c) => -color * c.score }) {
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

step(depth = 4, color = 1)
*/