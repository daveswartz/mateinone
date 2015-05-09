import mateinone._
import TerminalPrinter._

case class Score(value: Int, moves: List[(MoveBase, Int)], evaluations: Int)

def alphaBetaMax(board: BoardWithEvaluator, alpha: Int, beta: Int, depth: Int): Score =
  if (depth == 0) {
    Score(board.evaluation, Nil, 1)
  } else {
    var maxAlpha = Score(alpha, Nil, 0)
    for ((childMove, childBoard) <- board.leaves) {
      val score = alphaBetaMin(childBoard, maxAlpha.value, beta, depth - 1)
      if (score.value >= beta) return Score(beta, (childMove, childBoard.evaluation) :: score.moves, maxAlpha.evaluations + score.evaluations) // fail hard beta-cutoff
      if (score.value > maxAlpha.value) maxAlpha = Score(score.value, (childMove, childBoard.evaluation) :: score.moves, maxAlpha.evaluations + score.evaluations) // alpha acts like max in MiniMax
      else maxAlpha = maxAlpha.copy(evaluations = maxAlpha.evaluations + score.evaluations)
    }
    maxAlpha
  }

def alphaBetaMin(board: BoardWithEvaluator, alpha: Int, beta: Int, depth: Int): Score =
  if (depth == 0) {
    Score(board.evaluation, Nil, 1)
  } else {
    var minBeta = Score(beta, Nil, 0)
    for ((childMove, childBoard) <- board.leaves) {
      val score = alphaBetaMax(childBoard, alpha, minBeta.value, depth - 1)
      if (score.value <= alpha) return Score(alpha, (childMove, childBoard.evaluation) :: score.moves, minBeta.evaluations + score.evaluations) // fail hard alpha-cutoff
      if (score.value < minBeta.value) minBeta = Score(score.value, (childMove, childBoard.evaluation) :: score.moves, minBeta.evaluations + score.evaluations) // beta acts like min in MiniMax
      else minBeta = minBeta.copy(evaluations = minBeta.evaluations + score.evaluations)
    }
    minBeta
  }

def next(b: BoardWithEvaluator, depth: Int): Score =
  if (b.b.same.color == White) alphaBetaMax(b, Int.MinValue, Int.MaxValue, depth)
  else alphaBetaMin(b, Int.MinValue, Int.MaxValue, depth)

def step(board: Board, depth: Int, n: Int): Unit =
  if (board.isCheckmate) println(s"Checkmate ${board.opponent.color.toString} wins")
  else if (board.isStalemate) println("Stalemate")
  else if (board.isInsufficientMaterial) println("Insufficient mating material")
  else if (board.isThreefoldRepetition) println(s"${board.same.color.toString} claimed draw by threefold repetition")
  else if (board.isFiftyMoveRule) println(s"${board.same.color.toString} claimed draw by fifty-move rule")
  else {
    val score @ Score(_, (nextMove, _) :: _, _) = next(BoardWithEvaluator(board), depth)
    val nextBoard = board.move(nextMove).get
    println(nextBoard.print(nextMove))
    println(s"${n / 2 + 1}. $nextMove | $score")
    step(nextBoard, depth, n + 1)
  }

step(Board.initial, 4, 0)
