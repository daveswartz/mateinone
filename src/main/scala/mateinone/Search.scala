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

  // Optimization: TT metrics
  var ttHits = 0L
  var nodesSearched = 0L

  // Optimization: Killer moves (2 per ply)
  private val MaxSearchDepth = 64
  private val killerMoves = Array.fill(MaxSearchDepth, 2)(Option.empty[MoveBase])

  private def isKiller(m: MoveBase, ply: Int): Boolean = {
    if (ply >= MaxSearchDepth) false
    else killerMoves(ply)(0).contains(m) || killerMoves(ply)(1).contains(m)
  }

  private def storeKiller(m: MoveBase, ply: Int): Unit = {
    if (ply < MaxSearchDepth) {
      if (!killerMoves(ply)(0).contains(m)) {
        killerMoves(ply)(1) = killerMoves(ply)(0)
        killerMoves(ply)(0) = Some(m)
      }
    }
  }

  def scoreMove(board: Board, move: MoveBase, ply: Int): Int = move match {
    case m: StartAndEnd =>
      val aggressor = board.same.typeAt(m.start).getOrElse(Pawn)
      val victimOpt = board.opponent.typeAt(m.end)
      victimOpt match {
        case Some(victim) => (pieceValues(victim) * 10) - pieceValues(aggressor) + 2000 // Captures highest
        case None => 
          if (isKiller(move, ply)) 1000 // Killer moves after captures
          else 0
      }
    case _: Castle => 0
  }

  def nextMove(board: Board, depth: Int, evaluator: Evaluator, alphaBetaPruning: Boolean = true): SearchScore = {
    ttHits = 0
    nodesSearched = 0
    // Reset killers for new root search
    for (i <- 0 until MaxSearchDepth; j <- 0 until 2) killerMoves(i)(j) = None

    if (board.same.color == White) alphaBetaMax(board, depth, evaluator, alphaBetaPruning)
    else alphaBetaMin(board, depth, evaluator, alphaBetaPruning)
  }

  private def alphaBetaMax(board: Board, depth: Int, evaluator: Evaluator, pruning: Boolean): SearchScore = {
    def descending(leaves: Vector[(MoveBase, Board)], d: Int): Vector[(MoveBase, Board, SearchScore)] = {
      var maxAlpha = Int.MinValue
      val sortedLeaves = leaves.sortBy { case (m, b) => -scoreMove(board, m, 0) }
      sortedLeaves.map { case (childMove, childBoard) =>
        val childScore = alphaBetaMin(childBoard, maxAlpha, Int.MaxValue, d - 1, 1, evaluator, pruning)
        val finalScore = childScore.copy(moves = childMove :: childScore.moves)
        if (finalScore.score > maxAlpha) maxAlpha = finalScore.score
        (childMove, childBoard, finalScore)
      }.sortBy(-_._3.score)
    }

    var desc = descending(board.leaves, 1)
    2.to(depth).foreach(d => desc = descending(desc.map { case (m, b, s) => (m, b) }, d))
    desc.head._3
  }

  private def alphaBetaMin(board: Board, depth: Int, evaluator: Evaluator, pruning: Boolean): SearchScore = {
    def ascending(leaves: Vector[(MoveBase, Board)], d: Int): Vector[(MoveBase, Board, SearchScore)] = {
      var minBeta = Int.MaxValue
      val sortedLeaves = leaves.sortBy { case (m, b) => -scoreMove(board, m, 0) }
      sortedLeaves.map { case (childMove, childBoard) =>
        val childScore = alphaBetaMax(childBoard, Int.MinValue, minBeta, d - 1, 1, evaluator, pruning)
        val finalScore = childScore.copy(moves = childMove :: childScore.moves)
        if (finalScore.score < minBeta) minBeta = finalScore.score
        (childMove, childBoard, finalScore)
      }.sortBy(_._3.score)
    }

    var asc = ascending(board.leaves, 1)
    2.to(depth).foreach(d => asc = ascending(asc.map { case (m, b, s) => (m, b) }, d))
    asc.head._3
  }

  // Mate score thresholds
  private val MateScore = 20000
  private val MateThreshold = 15000 // If score > this, it's a mate score

  private def scoreToTT(score: Int, ply: Int): Int = {
    if (score > MateThreshold) score + ply
    else if (score < -MateThreshold) score - ply
    else score
  }

  private def scoreFromTT(score: Int, ply: Int): Int = {
    if (score > MateThreshold) score - ply
    else if (score < -MateThreshold) score + ply
    else score
  }

  private def alphaBetaMax(board: Board, alpha: Int, beta: Int, depth: Int, ply: Int, evaluator: Evaluator, pruning: Boolean): SearchScore = {
    nodesSearched += 1
    if (board.isThreefoldRepetition) return SearchScore(0, Nil)

    // 1. TT Lookup
    val ttEntry = TranspositionTable.get(board.hash)
    var ttMove: Option[MoveBase] = None
    
    if (ttEntry.isDefined) {
      val entry = ttEntry.get
      ttMove = entry.bestMove
      if (entry.depth >= depth) {
        ttHits += 1
        val ttScore = scoreFromTT(entry.score, ply)
        if (entry.flag == TranspositionTable.Exact) return SearchScore(ttScore, ttMove.toList)
        if (entry.flag == TranspositionTable.LowerBound && ttScore >= beta) return SearchScore(ttScore, ttMove.toList)
        if (entry.flag == TranspositionTable.UpperBound && ttScore <= alpha) return SearchScore(ttScore, ttMove.toList)
      }
    }

    if (depth == 0) return SearchScore(evaluator.evaluate(board, ply), Nil)

    // 2. Null Move Pruning
    if (pruning && depth >= 3 && !board.isCheck && ply > 0) {
      // R = 2
      val nullBoard = board.copy(same = board.opponent, opponent = board.same, hash = board.hash ^ Zobrist.sideToMove)
      val nullScore = alphaBetaMin(nullBoard, -beta, -beta + 1, depth - 3, ply + 1, evaluator, pruning)
      if (-nullScore.score >= beta) return SearchScore(beta, Nil)
    }

    val leaves = board.leaves
    if (leaves.isEmpty) return SearchScore(evaluator.evaluate(board, ply), Nil)

    // 3. Move Ordering
    val sortedLeaves = leaves.sortBy { case (m, b) => 
      if (ttMove.contains(m)) Int.MaxValue 
      else -scoreMove(board, m, ply) 
    }

    var maxAlpha = alpha
    var bestMoves = List.empty[MoveBase]
    var bestMoveBase: Option[MoveBase] = None
    var flag = TranspositionTable.UpperBound

    for ((childMove, childBoard) <- sortedLeaves) {
      val childScore = alphaBetaMin(childBoard, maxAlpha, beta, depth - 1, ply + 1, evaluator, pruning)
      
      if (pruning && childScore.score >= beta) {
        storeKiller(childMove, ply)
        TranspositionTable.store(board.hash, depth, scoreToTT(beta, ply), TranspositionTable.LowerBound, Some(childMove))
        return SearchScore(beta, Nil)
      }
      
      if (childScore.score > maxAlpha) {
        maxAlpha = childScore.score
        bestMoves = childMove :: childScore.moves
        bestMoveBase = Some(childMove)
        flag = TranspositionTable.Exact
      }
    }

    TranspositionTable.store(board.hash, depth, scoreToTT(maxAlpha, ply), flag, bestMoveBase)
    SearchScore(maxAlpha, bestMoves)
  }

  private def alphaBetaMin(board: Board, alpha: Int, beta: Int, depth: Int, ply: Int, evaluator: Evaluator, pruning: Boolean): SearchScore = {
    nodesSearched += 1
    if (board.isThreefoldRepetition) return SearchScore(0, Nil)

    // 1. TT Lookup
    val ttEntry = TranspositionTable.get(board.hash)
    var ttMove: Option[MoveBase] = None

    if (ttEntry.isDefined) {
      val entry = ttEntry.get
      ttMove = entry.bestMove
      if (entry.depth >= depth) {
        ttHits += 1
        val ttScore = scoreFromTT(entry.score, ply)
        if (entry.flag == TranspositionTable.Exact) return SearchScore(ttScore, ttMove.toList)
        if (entry.flag == TranspositionTable.LowerBound && ttScore >= beta) return SearchScore(ttScore, ttMove.toList)
        if (entry.flag == TranspositionTable.UpperBound && ttScore <= alpha) return SearchScore(ttScore, ttMove.toList)
      }
    }

    if (depth == 0) return SearchScore(evaluator.evaluate(board, ply), Nil)

    // 2. Null Move Pruning
    if (pruning && depth >= 3 && !board.isCheck && ply > 0) {
      val nullBoard = board.copy(same = board.opponent, opponent = board.same, hash = board.hash ^ Zobrist.sideToMove)
      val nullScore = alphaBetaMax(nullBoard, -alpha - 1, -alpha, depth - 3, ply + 1, evaluator, pruning)
      if (-nullScore.score <= alpha) return SearchScore(alpha, Nil)
    }

    val leaves = board.leaves
    if (leaves.isEmpty) return SearchScore(evaluator.evaluate(board, ply), Nil)

    // 3. Move Ordering
    val sortedLeaves = leaves.sortBy { case (m, b) => 
      if (ttMove.contains(m)) Int.MaxValue 
      else -scoreMove(board, m, ply) 
    }

    var minBeta = beta
    var bestMoves = List.empty[MoveBase]
    var bestMoveBase: Option[MoveBase] = None
    var flag = TranspositionTable.LowerBound

    for ((childMove, childBoard) <- sortedLeaves) {
      val childScore = alphaBetaMax(childBoard, alpha, minBeta, depth - 1, ply + 1, evaluator, pruning)
      
      if (pruning && childScore.score <= alpha) {
        storeKiller(childMove, ply)
        TranspositionTable.store(board.hash, depth, scoreToTT(alpha, ply), TranspositionTable.UpperBound, Some(childMove))
        return SearchScore(alpha, Nil)
      }
      
      if (childScore.score < minBeta) {
        minBeta = childScore.score
        bestMoves = childMove :: childScore.moves
        bestMoveBase = Some(childMove)
        flag = TranspositionTable.Exact
      }
    }

    TranspositionTable.store(board.hash, depth, scoreToTT(minBeta, ply), flag, bestMoveBase)
    SearchScore(minBeta, bestMoves)
  }
}
