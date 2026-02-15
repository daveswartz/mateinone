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

  def scoreMove(b: Bitboard, m: Move, ply: Int): Int = {
    if (m.capture) {
      val victimType = b.pieceAt(m.to)
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

    if (depth <= 0) return evaluate(b)

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

    val moves = MoveGen.generateMoves(b).sortBy(m => -scoreMove(b, m, ply))
    var maxAlpha = alpha
    var flag = TranspositionTable.UpperBound
    var bestMove: Option[Move] = None
    var legalMoves = 0

    for ((m, i) <- moves.zipWithIndex) {
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

  private def evaluate(b: Bitboard): Int = {
    var score = 0
    for (pt <- 0 to 4) {
      var wbb = b.pieceBB(White)(pt)
      while (wbb != 0) { score += pieceValues(pt); wbb &= (wbb - 1) }
      var bbb = b.pieceBB(Black)(pt)
      while (bbb != 0) { score -= pieceValues(pt); bbb &= (bbb - 1) }
    }
    if (b.sideToMove == White) score else -score
  }
}
