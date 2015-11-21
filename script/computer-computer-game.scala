import scala.language.reflectiveCalls
import mateinone._
import TerminalPrinter._
import mateinone.evaluators.{Evaluator, Simplified}

val alphaBetaPruning = true
val iterativeDeepening = true
val lookAheadDepth = 5
val evaluator: Evaluator = Simplified

case class Score(score: Int, moves: List[MoveBase])

var numEvaluations = 0
def evaluate(board: Board) = {
  numEvaluations += 1
  evaluator.evaluate(board)
}

def alphaBetaMax(board: Board, depth: Int): Score = {
  assert(depth > 0)

  def descending(leaves: Vector[(MoveBase, Board)], d: Int) = {
    val alpha = Int.MinValue
    val beta = Int.MaxValue
    var maxAlpha = Score(alpha, Nil)
    leaves.map { case (childMove, childBoard) =>
      var childScore = alphaBetaMin(childBoard, maxAlpha.score, beta, d - 1)
      childScore = childScore.copy(moves = childMove :: childScore.moves)
      if (childScore.score > maxAlpha.score) maxAlpha = childScore
      (childMove, childBoard, childScore)
    }.sortBy(-_._3.score)
  }

  if (iterativeDeepening) {
    var desc = descending(board.leaves, 1)
    2.to(depth).foreach(d => desc = descending(desc.map { case (m, b, s) => (m, b) }, d))
    desc.head._3
  } else {
    descending(board.leaves, depth).head._3
  }
}

def alphaBetaMin(board: Board, depth: Int): Score = {
  assert(depth > 0)

  def ascending(leaves: Vector[(MoveBase, Board)], d: Int) = {
    val alpha = Int.MinValue
    val beta = Int.MaxValue
    var minBeta = Score(beta, Nil)
    leaves.map { case (childMove, childBoard) =>
      var childScore = alphaBetaMax(childBoard, alpha, minBeta.score, d - 1)
      childScore = childScore.copy(moves = childMove :: childScore.moves)
      if (childScore.score < minBeta.score) minBeta = childScore
      (childMove, childBoard, childScore)
    }.sortBy(_._3.score)
  }

  if (iterativeDeepening) {
	var asc = ascending(board.leaves, 1)
	2.to(depth).foreach(d => asc = ascending(asc.map { case (m, b, s) => (m, b) }, d))
	asc.head._3
  } else {
    ascending(board.leaves, depth).head._3
  }
}

def alphaBetaMax(board: Board, alpha: Int, beta: Int, depth: Int): Score = {
  val eval = evaluate(board)
  if (depth == 0) {
    Score(eval, Nil)
  } else {
    var maxAlpha = Score(alpha, Nil)
    for ((childMove, childBoard) <- board.leaves) {
      val childScore = alphaBetaMin(childBoard, maxAlpha.score, beta, depth - 1)
      if (alphaBetaPruning && childScore.score >= beta) // fail hard beta-cutoff
        return Score(beta, Nil)
      if (childScore.score > maxAlpha.score) // alpha acts like max in MiniMax
        maxAlpha = Score(childScore.score, childMove :: childScore.moves)
    }
    maxAlpha
  }
}

def alphaBetaMin(board: Board, alpha: Int, beta: Int, depth: Int): Score = {
  val eval = evaluate(board)
  if (depth == 0) {
    Score(eval, Nil)
  } else {
    var minBeta = Score(beta, Nil)
    for ((childMove, childBoard) <- board.leaves) {
      val childScore = alphaBetaMax(childBoard, alpha, minBeta.score, depth - 1)
      if (alphaBetaPruning && childScore.score <= alpha) // fail hard alpha-cutoff
        return Score(alpha, Nil)
      if (childScore.score < minBeta.score) // beta acts like min in MiniMax
        minBeta = Score(childScore.score, childMove :: childScore.moves)
    }
    minBeta
  }
}

def next(b: Board, depth: Int) = if (b.same.color == White) alphaBetaMax(b, depth) else alphaBetaMin(b, depth)

def step(board: Board, depth: Int, n: Int): Unit = {
  if (board.isCheckmate) println(s"Checkmate ${board.opponent.color.toString} wins")
  else if (board.isStalemate) println("Stalemate")
  else if (board.isInsufficientMaterial) println("Insufficient mating material")
  else if (board.isThreefoldRepetition) println(s"${board.same.color.toString} claimed draw by threefold repetition")
  else if (board.isFiftyMoveRule) println(s"${board.same.color.toString} claimed draw by fifty-move rule")
  else {
    numEvaluations = 0
    val start = System.nanoTime()
    val score = next(board, depth)
    val delta = (System.nanoTime() - start) / 1e9

    def isWhite(i: Int) = i%2 == 0
    def whitePrefix(i: Int) = s"${i/2+1}."
    def blackPrefix(i: Int) = s"${whitePrefix(i)} ..."
    def prefix(i: Int) = if (isWhite(i)) whitePrefix(i) else blackPrefix(i)
    def prefixIfWhite(i: Int) = if (isWhite(i)) whitePrefix(i) else ""

    val afterNextMoves = {
      val indices = (n+1).to(n+1+depth)
      val prefixes = prefix(indices.head) +: indices.tail.map(prefixIfWhite)
      score.moves.tail.zip(prefixes).map { case (m, p) => s"$p $m" }.mkString(" ")
    }

    val nextMove = score.moves.head
    val nextBoard = board.move(nextMove).get
    println(nextBoard.print(nextMove))
    println(s"${prefix(n)} $nextMove")
    println(f"Score: ${score.score/100f}%+.2f $afterNextMoves")
    println(f"Engine: $numEvaluations%,d in $delta%.2fs")

    step(nextBoard, depth, n + 1)
  }
}

println(s"alphaBetaPruning: $alphaBetaPruning")
println(s"iterativeDeepening: $iterativeDeepening")
println(s"lookAheadDepth: $lookAheadDepth")
step(Board.initial, lookAheadDepth, 0)
