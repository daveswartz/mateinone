import scala.language.reflectiveCalls
import mateinone._
import TerminalPrinter._
import mateinone.evaluators.{Evaluator, Simplified}

val alphaBetaPruning = true
val lookAheadDepth = 5
val evaluator: Evaluator = Simplified

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

def step(board: Board, depth: Int, n: Int): Unit = {
  if (board.isCheckmate) println(s"Checkmate ${board.opponent.color.toString} wins")
  else if (board.isStalemate) println("Stalemate")
  else if (board.isInsufficientMaterial) println("Insufficient mating material")
  else if (board.isThreefoldRepetition) println(s"${board.same.color.toString} claimed draw by threefold repetition")
  else if (board.isFiftyMoveRule) println(s"${board.same.color.toString} claimed draw by fifty-move rule")
  else {
    val start = System.nanoTime()
    val score = next(board, depth)
    val delta = (System.nanoTime() - start) / 1e9

    def isWhite(i: Integer) = i%2 == 0
    def whitePrefix(i: Integer) = s"${i/2+1}."
    def blackPrefix(i: Integer) = s"${whitePrefix(i)} ..."
    def prefix(i: Integer) = if (isWhite(i)) whitePrefix(i) else blackPrefix(i)
    def prefixIfWhite(i: Integer) = if (isWhite(i)) whitePrefix(i) else ""

    val afterNextMoves = {
      val indices = (n+1).to(n+1+depth)
      val prefixes = prefix(indices.head) +: indices.tail.map(prefixIfWhite(_))
      score.moves.tail.map(_._1).zip(prefixes).map { case (m, p) => s"$p $m" }.mkString(" ")
    }

    val (nextMove, _) = score.moves.head
    val nextBoard = board.move(nextMove).get
    println(nextBoard.print(nextMove))
    println(s"${prefix(n)} $nextMove")
    println(f"Score: ${score.score/100f}%+.2f $afterNextMoves")
    println(f"Engine: ${score.nEvals}%,d in $delta%.2fs")

    step(nextBoard, depth, n + 1)
  }
}

step(Board.initial, lookAheadDepth, 0)
