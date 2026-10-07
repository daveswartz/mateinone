package mateinone.bitboard

import Constants._
import mateinone.TranspositionTable
import mateinone.Zobrist

object BitboardSearch {
  
  var nodesSearched = 0L
  var ttHits = 0L

  // The best move the last search found at the root, or 0 if it found none.
  var rootBestMove = 0

  // The game's positions before the root, as positionHistory held them when the search started.
  private var historyBeforeRoot: List[Long] = Nil

  // Checked every 2048 nodes, about a millisecond: once it's true, the search stops, and stopped
  // says so. A stopped search's scores and moves are partial, so they're for throwing away.
  var shouldStop: () => Boolean = () => false
  var stopped = false

  // The ms to spend on a move with timeLeft on the clock: an even share of it for the moves to go,
  // plus the increment, but never the last 50 ms, since the increment comes only after the move.
  def timeForMove(timeLeft: Long, increment: Long, movesToGo: Long): Long =
    Math.max(0, Math.min(timeLeft / Math.max(movesToGo, 1) + increment, timeLeft - 50))

  private val pieceValues = Array(100, 320, 330, 500, 900, 0)
  
  // Killer moves (2 per ply)
  private val MaxPly = 64
  private val killers = Array.fill(MaxPly, 2)(0)

  // The deepest search the killers have room for: it uses them at plies 0 to depth - 1.
  val MaxDepth = MaxPly
  
  // History table [color][from][to]
  private val history = Array.ofDim[Int](2, 64, 64)

  def clearHistory(): Unit = {
    for (c <- 0 to 1; f <- 0 until 64; t <- 0 until 64) history(c)(f)(t) = 0
    for (p <- 0 until MaxPly; i <- 0 until 2) killers(p)(i) = 0
  }

  def getPV(b: Bitboard, depth: Int): List[Int] = {
    if (depth <= 0) return Nil
    TranspositionTable.get(b.hash) match {
      case Some(entry) if entry.bestMove.isDefined =>
        val m = entry.bestMove.get.asInstanceOf[Int]
        b.makeMove(m)
        val rest = getPV(b, depth - 1)
        b.unmakeMove(m)
        m :: rest
      case _ => Nil
    }
  }

  def formatScore(score: Int): String = {
    if (score > 15000) s"Mate in ${(20000 - score + 1) / 2}"
    else if (score < -15000) s"Mate in ${(20000 + score + 1) / 2}"
    else f"${score / 100.0}%+.2f"
  }

  def scoreMove(b: Bitboard, m: Int, ply: Int, ttMove: Int): Int = {
    if (m == ttMove) 1000000 
    else if (mCapture(m)) {
      val victimType = if (mEP(m)) Pawn else b.pieceAt(mTo(m))
      (pieceValues(victimType) * 10) - pieceValues(mPiece(m)) + 20000
    } else {
      if (m == killers(ply)(0)) 9000
      else if (m == killers(ply)(1)) 8000
      else history(b.sideToMove)(mFrom(m))(mTo(m))
    }
  }

  private def countNode(): Unit = {
    nodesSearched += 1
    if ((nodesSearched & 2047) == 0 && shouldStop()) stopped = true
  }

  def search(b: Bitboard, depth: Int, alpha: Int, beta: Int, ply: Int = 0): Int = {
    countNode()
    if (stopped) return 0
    if (ply == 0) {
      rootBestMove = 0
      historyBeforeRoot = b.positionHistory
    }

    // The root has to choose a move even in a drawn position, so only the positions below it are
    // scored as repeats, as in Stockfish.
    if (ply > 0 && isRepetition(b)) return 0

    val ttEntry = TranspositionTable.get(b.hash)
    var ttMove = 0

    if (ttEntry.isDefined) {
      val entry = ttEntry.get
      ttMove = entry.bestMove.collect { case m: Int => m }.getOrElse(0)
      // A stored result can't answer at the root, which has to search its moves to choose one.
      if (entry.depth >= depth && ply > 0) {
        ttHits += 1
        if (entry.flag == TranspositionTable.Exact) return entry.score
        if (entry.flag == TranspositionTable.LowerBound && entry.score >= beta) return beta
        if (entry.flag == TranspositionTable.UpperBound && entry.score <= alpha) return alpha
      }
    }

    if (depth <= 0) return quiesce(b, alpha, beta, ply)

    if (depth >= 3 && !LegalChecker.isInCheck(b, b.sideToMove) && ply > 0) {
      val oldHash = b.hash
      val oldEp = b.enPassantSq
      b.sideToMove ^= 1
      b.hash ^= Zobrist.sideToMove
      if (b.enPassantSq != SquareNone) b.hash ^= Zobrist.enPassant(fileOf(b.enPassantSq))
      b.enPassantSq = SquareNone
      
      val score = -search(b, depth - 3, -beta, -beta + 1, ply + 1)
      
      b.sideToMove ^= 1
      b.hash = oldHash
      b.enPassantSq = oldEp
      
      if (score >= beta) return beta
    }

    val moves = MoveGen.generateMoves(b)
    val scoredMoves = new Array[Long](moves.length)
    for (i <- 0 until moves.length) {
      val m = moves(i)
      val s = scoreMove(b, m, ply, ttMove)
      // Pack score and move into Long for fast sorting (Score in upper 32 bits)
      scoredMoves(i) = (s.toLong << 32) | (i.toLong)
    }
    
    // Sort moves (Selection sort for simplicity and speed on small arrays)
    for (i <- 0 until scoredMoves.length) {
      var maxIdx = i
      for (j <- i + 1 until scoredMoves.length) {
        if (scoredMoves(j) > scoredMoves(maxIdx)) maxIdx = j
      }
      val temp = scoredMoves(i)
      scoredMoves(i) = scoredMoves(maxIdx)
      scoredMoves(maxIdx) = temp
    }

    var maxAlpha = alpha
    var flag = TranspositionTable.UpperBound
    var bestMove = 0
    var legalMoves = 0

    for (i <- 0 until scoredMoves.length) {
      val m = moves((scoredMoves(i) & 0xFFFFFFFFL).toInt)
      if (b.pieceAt(mTo(m)) == King) return 30000 

      b.makeMove(m)
      if (LegalChecker.isInCheck(b, b.sideToMove ^ 1)) {
        b.unmakeMove(m)
      } else {
        legalMoves += 1
        var score = 0
        if (depth >= 3 && legalMoves > 4 && !mCapture(m) && mPromo(m) == PieceNone && !LegalChecker.isInCheck(b, b.sideToMove)) {
          score = -search(b, depth - 2, -maxAlpha - 1, -maxAlpha, ply + 1)
          if (score > maxAlpha) {
            score = -search(b, depth - 1, -beta, -maxAlpha, ply + 1)
          }
        } else {
          score = -search(b, depth - 1, -beta, -maxAlpha, ply + 1)
        }
        
        b.unmakeMove(m)
        if (stopped) return 0
        
        if (score >= beta) {
          if (!mCapture(m)) {
            killers(ply)(1) = killers(ply)(0)
            killers(ply)(0) = m
            history(b.sideToMove)(mFrom(m))(mTo(m)) += depth * depth
          }
          TranspositionTable.store(b.hash, depth, beta, TranspositionTable.LowerBound, Some(m))
          if (ply == 0) rootBestMove = m
          return beta
        }
        if (score > maxAlpha) {
          maxAlpha = score
          bestMove = m
          flag = TranspositionTable.Exact
          if (ply == 0) rootBestMove = m
        }
      }
    }

    if (legalMoves == 0) return if (LegalChecker.isInCheck(b, b.sideToMove)) -20000 + ply else 0

    TranspositionTable.store(b.hash, depth, maxAlpha, flag, if (bestMove != 0) Some(bestMove) else None)
    maxAlpha
  }

  // Stockfish's rule: a position is a draw if it repeats once strictly after the root, or twice
  // in all. The side that steered into a repeat since the root can repeat it again. Only for
  // positions below the root.
  private def isRepetition(b: Bitboard): Boolean = {
    if (b.isThreefoldRepetition) return true
    // positionHistory is newest first, and each move pushes the position it left, so the entries
    // ahead of the root's own are the positions reached since the root.
    var h = b.positionHistory
    while (h.nonEmpty && (h.tail ne historyBeforeRoot)) {
      if (h.head == b.hash) return true
      h = h.tail
    }
    false
  }

  def quiesce(b: Bitboard, alpha: Int, beta: Int, ply: Int): Int = {
    countNode()
    if (stopped) return 0
    val standingPat = BitboardEvaluator.evaluate(b, ply)
    if (standingPat >= beta) return beta
    var maxAlpha = Math.max(alpha, standingPat)

    val captures = MoveGen.generateCaptures(b)
    val scored = new Array[Long](captures.length)
    for (i <- 0 until captures.length) {
      scored(i) = (scoreMove(b, captures(i), ply, 0).toLong << 32) | i.toLong
    }
    // Simple sort
    for (i <- 0 until scored.length) {
      var maxIdx = i
      for (j <- i + 1 until scored.length) if (scored(j) > scored(maxIdx)) maxIdx = j
      val t = scored(i); scored(i) = scored(maxIdx); scored(maxIdx) = t
    }

    for (i <- 0 until scored.length) {
      val m = captures((scored(i) & 0xFFFFFFFFL).toInt)
      if (b.pieceAt(mTo(m)) == King) return 30000
      b.makeMove(m)
      if (LegalChecker.isInCheck(b, b.sideToMove ^ 1)) {
        b.unmakeMove(m)
      } else {
        val score = -quiesce(b, -beta, -maxAlpha, ply + 1)
        b.unmakeMove(m)
        if (score >= beta) return beta
        if (score > maxAlpha) maxAlpha = score
      }
    }
    maxAlpha
  }
}
