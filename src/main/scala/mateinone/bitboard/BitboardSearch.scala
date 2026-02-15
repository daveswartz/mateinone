package mateinone.bitboard

import Constants._
import mateinone.TranspositionTable
import mateinone.Zobrist

object BitboardSearch {
  
  var nodesSearched = 0L
  var ttHits = 0L

  private val pieceValues = Array(100, 320, 330, 500, 900, 0)
  
  // Killer moves (2 per ply)
  private val MaxPly = 64
  private val killers = Array.fill(MaxPly, 2)(null: Move)

  def getPV(b: Bitboard, depth: Int): List[Move] = {
    if (depth <= 0) return Nil
    TranspositionTable.get(b.hash) match {
      case Some(entry) if entry.bestMove.isDefined =>
        val m = entry.bestMove.get.asInstanceOf[Move]
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

  def scoreMove(b: Bitboard, m: Move, ply: Int, ttMove: Option[Move]): Int = {
    if (ttMove.contains(m)) 1000000 
    else if (m.capture) {
      val victimType = if (m.enPassant) Pawn else b.pieceAt(m.to)
      (pieceValues(victimType) * 10) - pieceValues(m.piece) + 20000
    } else {
      if (m == killers(ply)(0)) 9000
      else if (m == killers(ply)(1)) 8000
      else 0
    }
  }

  def search(b: Bitboard, depth: Int, alpha: Int, beta: Int, ply: Int = 0): Int = {
    nodesSearched += 1
    
    // Repetition check
    if (b.isThreefoldRepetition) return 0

    // 1. TT Lookup
    val ttEntry = TranspositionTable.get(b.hash)
    var ttMove: Option[Move] = None

    if (ttEntry.isDefined) {
      val entry = ttEntry.get
      ttMove = entry.bestMove.collect { case m: Move => m }
      if (entry.depth >= depth) {
        ttHits += 1
        if (entry.flag == TranspositionTable.Exact) return entry.score
        if (entry.flag == TranspositionTable.LowerBound && entry.score >= beta) return beta
        if (entry.flag == TranspositionTable.UpperBound && entry.score <= alpha) return alpha
      }
    }

    if (depth <= 0) {
      return quiesce(b, alpha, beta, ply)
    }

    // 2. Null Move Pruning
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

    val moves = MoveGen.generateMoves(b).sortBy(m => -scoreMove(b, m, ply, ttMove))
    var maxAlpha = alpha
    var flag = TranspositionTable.UpperBound
    var bestMove: Option[Move] = None
    var legalMoves = 0

    for ((m, i) <- moves.zipWithIndex) {
      // Fast check: if move captures a king, return immediately (should be caught by LegalChecker but for safety)
      if (b.pieceAt(m.to) == King) return 30000 

      b.makeMove(m)
      
      if (LegalChecker.isInCheck(b, b.sideToMove ^ 1)) {
        b.unmakeMove(m)
      } else {
        legalMoves += 1
        
        var score = 0
        // 3. Late Move Reductions (LMR)
        if (depth >= 3 && legalMoves > 4 && !m.capture && m.promo == PieceNone && !LegalChecker.isInCheck(b, b.sideToMove)) {
          score = -search(b, depth - 2, -maxAlpha - 1, -maxAlpha, ply + 1)
          if (score > maxAlpha) { // Re-search
            score = -search(b, depth - 1, -beta, -maxAlpha, ply + 1)
          }
        } else {
          score = -search(b, depth - 1, -beta, -maxAlpha, ply + 1)
        }
        
        b.unmakeMove(m)
        
        if (score >= beta) {
          if (!m.capture) {
            killers(ply)(1) = killers(ply)(0)
            killers(ply)(0) = m
          }
          TranspositionTable.store(b.hash, depth, beta, TranspositionTable.LowerBound, Some(m))
          return beta
        }
        if (score > maxAlpha) {
          maxAlpha = score
          bestMove = Some(m)
          flag = TranspositionTable.Exact
        }
      }
    }

    if (legalMoves == 0) {
      return if (LegalChecker.isInCheck(b, b.sideToMove)) -20000 + ply else 0
    }

    TranspositionTable.store(b.hash, depth, maxAlpha, flag, bestMove)
    maxAlpha
  }

  def quiesce(b: Bitboard, alpha: Int, beta: Int, ply: Int): Int = {
    nodesSearched += 1
    val standingPat = BitboardEvaluator.evaluate(b, ply)
    if (standingPat >= beta) return beta
    var maxAlpha = Math.max(alpha, standingPat)

    val captures = MoveGen.generateCaptures(b).sortBy(m => -scoreMove(b, m, ply, None))
    for (m <- captures) {
      if (b.pieceAt(m.to) == King) return 30000

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
