import mateinone._
import TerminalPrinter._
import mateinone.evaluators.{Evaluator, Simplified}

val alphaBetaPruning = true
val lookAheadDepth = 5
val evaluator: Evaluator = Simplified

// TODO restore delta evaluation as option (on/off boolean) if speeds up/does not affect result

case class Score(score: Int, moves: List[(MoveBase, Int)], nEvals: Int) {
  def addEvals(e: Int) = copy(nEvals = nEvals + e)
}

def alphaBetaMax(move: MoveBase, board: Board, alpha: Int, beta: Int, depth: Int): Score = {
  val eval = evaluator.evaluate(board)
  var nEvals = 1
  if (depth == 0) {
    Score(eval, (move, eval) :: Nil, nEvals)
  } else {
    var maxAlpha = Score(alpha, Nil, nEvals)
    for ((childMove, childBoard) <- board.leaves) {
      val childScore = alphaBetaMin(childMove, childBoard, maxAlpha.score, beta, depth - 1)
      nEvals += childScore.nEvals
      if (alphaBetaPruning && childScore.score >= beta) // fail hard beta-cutoff
        return Score(beta, Nil, nEvals)
      if (childScore.score > maxAlpha.score) // alpha acts like max in MiniMax
        maxAlpha = Score(childScore.score, (move, eval) :: childScore.moves, nEvals)
    }
    maxAlpha.copy(nEvals = nEvals)
  }
}

def alphaBetaMin(move: MoveBase, board: Board, alpha: Int, beta: Int, depth: Int): Score = {
  val eval = evaluator.evaluate(board)
  var nEvals = 1
  if (depth == 0) {
    Score(eval, (move, eval) :: Nil, nEvals)
  } else {
    var minBeta = Score(beta, Nil, nEvals)
    for ((childMove, childBoard) <- board.leaves) {
      val childScore = alphaBetaMax(childMove, childBoard, alpha, minBeta.score, depth - 1)
      nEvals += childScore.nEvals
      if (alphaBetaPruning && childScore.score <= alpha) // fail hard alpha-cutoff
        return Score(alpha, Nil, nEvals)
      if (childScore.score < minBeta.score) // beta acts like min in MiniMax
        minBeta = Score(childScore.score, (move, eval) :: childScore.moves, nEvals)
    }
    minBeta.copy(nEvals = nEvals)
  }
}

def next(b: Board, depth: Int): Score = {
  val score =
    if (b.same.color == White) alphaBetaMax(null, b, Int.MinValue, Int.MaxValue, depth)
    else alphaBetaMin(null, b, Int.MinValue, Int.MaxValue, depth)
  score.copy(moves = score.moves.tail)
}

def step(board: Board, depth: Int, n: Int): Unit =
  if (board.isCheckmate) println(s"Checkmate ${board.opponent.color.toString} wins")
  else if (board.isStalemate) println("Stalemate")
  else if (board.isInsufficientMaterial) println("Insufficient mating material")
  else if (board.isThreefoldRepetition) println(s"${board.same.color.toString} claimed draw by threefold repetition")
  else if (board.isFiftyMoveRule) println(s"${board.same.color.toString} claimed draw by fifty-move rule")
  else {
    val start = System.nanoTime()
    val score = next(board, depth)
    val deltaInSeconds = (System.nanoTime() - start) / 1e9
    val (nextMove, _) = score.moves.head
    val nextBoard = board.move(nextMove).get
    println(nextBoard.print(nextMove))
    println(s"${n / 2 + 1}. $nextMove | $score | $deltaInSeconds seconds")
    step(nextBoard, depth, n + 1)
  }

step(Board.initial, lookAheadDepth, 0)
