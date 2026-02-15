package mateinone

import mateinone.evaluators.Evaluator

case class SearchScore(score: Int, moves: List[MoveBase])

object Search {
  private val pieceValues = Map[PieceType, Int](
    Pawn -> 1,
    Knight -> 3,
    Bishop -> 3,
    Rook -> 5,
    Queen -> 9,
    King -> 100
  )

  def scoreMove(board: Board, move: MoveBase): Int = move match {
    case m: StartAndEnd =>
      val aggressor = board.same.typeAt(m.start).getOrElse(Pawn)
      val victimOpt = board.opponent.typeAt(m.end)
      victimOpt match {
        case Some(victim) => (pieceValues(victim) * 10) - pieceValues(aggressor) + 1000
        case None => 0
      }
    case _: Castle => 0
  }

  def nextMove(board: Board, depth: Int, evaluator: Evaluator, alphaBetaPruning: Boolean = true): SearchScore = {
    if (board.same.color == White) alphaBetaMax(board, depth, evaluator, alphaBetaPruning)
    else alphaBetaMin(board, depth, evaluator, alphaBetaPruning)
  }

  private def alphaBetaMax(board: Board, depth: Int, evaluator: Evaluator, pruning: Boolean): SearchScore = {
    def descending(leaves: Vector[(MoveBase, Board)], d: Int): Vector[(MoveBase, Board, SearchScore)] = {
      var maxAlpha = Int.MinValue
      val sortedLeaves = leaves.sortBy { case (m, b) => -scoreMove(board, m) }
      sortedLeaves.map { case (childMove, childBoard) =>
        val childScore = alphaBetaMin(childBoard, maxAlpha, Int.MaxValue, d - 1, 1, evaluator, pruning)
        val finalScore = childScore.copy(moves = childMove :: childScore.moves)
        if (finalScore.score > maxAlpha) maxAlpha = finalScore.score
        (childMove, childBoard, finalScore)
      }.sortBy(-_._3.score)
    }

    val iterativeDeepening = true // Default internal behavior
    if (iterativeDeepening) {
      var desc = descending(board.leaves, 1)
      2.to(depth).foreach(d => desc = descending(desc.map { case (m, b, s) => (m, b) }, d))
      desc.head._3
    } else {
      descending(board.leaves, depth).head._3
    }
  }

  private def alphaBetaMin(board: Board, depth: Int, evaluator: Evaluator, pruning: Boolean): SearchScore = {
    def ascending(leaves: Vector[(MoveBase, Board)], d: Int): Vector[(MoveBase, Board, SearchScore)] = {
      var minBeta = Int.MaxValue
      val sortedLeaves = leaves.sortBy { case (m, b) => -scoreMove(board, m) }
      sortedLeaves.map { case (childMove, childBoard) =>
        val childScore = alphaBetaMax(childBoard, Int.MinValue, minBeta, d - 1, 1, evaluator, pruning)
        val finalScore = childScore.copy(moves = childMove :: childScore.moves)
        if (finalScore.score < minBeta) minBeta = finalScore.score
        (childMove, childBoard, finalScore)
      }.sortBy(_._3.score)
    }

    val iterativeDeepening = true
    if (iterativeDeepening) {
      var asc = ascending(board.leaves, 1)
      2.to(depth).foreach(d => asc = ascending(asc.map { case (m, b, s) => (m, b) }, d))
      asc.head._3
    } else {
      ascending(board.leaves, depth).head._3
    }
  }

  private def alphaBetaMax(board: Board, alpha: Int, beta: Int, depth: Int, ply: Int, evaluator: Evaluator, pruning: Boolean): SearchScore = {
    if (board.isThreefoldRepetition) return SearchScore(0, Nil)
    val score = SearchScore(evaluator.evaluate(board, ply), Nil)
    if (depth == 0 || board.leaves.isEmpty) return score

    val sortedLeaves = board.leaves.sortBy { case (m, b) => -scoreMove(board, m) }
    var maxAlpha = alpha
    var bestMoves = List.empty[MoveBase]

    for ((childMove, childBoard) <- sortedLeaves) {
      val childScore = alphaBetaMin(childBoard, maxAlpha, beta, depth - 1, ply + 1, evaluator, pruning)
      if (pruning && childScore.score >= beta) return SearchScore(beta, Nil)
      if (childScore.score > maxAlpha) {
        maxAlpha = childScore.score
        bestMoves = childMove :: childScore.moves
      }
    }
    SearchScore(maxAlpha, bestMoves)
  }

  private def alphaBetaMin(board: Board, alpha: Int, beta: Int, depth: Int, ply: Int, evaluator: Evaluator, pruning: Boolean): SearchScore = {
    if (board.isThreefoldRepetition) return SearchScore(0, Nil)
    val score = SearchScore(evaluator.evaluate(board, ply), Nil)
    if (depth == 0 || board.leaves.isEmpty) return score

    val sortedLeaves = board.leaves.sortBy { case (m, b) => -scoreMove(board, m) }
    var minBeta = beta
    var bestMoves = List.empty[MoveBase]

    for ((childMove, childBoard) <- sortedLeaves) {
      val childScore = alphaBetaMax(childBoard, alpha, minBeta, depth - 1, ply + 1, evaluator, pruning)
      if (pruning && childScore.score <= alpha) return SearchScore(alpha, Nil)
      if (childScore.score < minBeta) {
        minBeta = childScore.score
        bestMoves = childMove :: childScore.moves
      }
    }
    SearchScore(minBeta, bestMoves)
  }
}
