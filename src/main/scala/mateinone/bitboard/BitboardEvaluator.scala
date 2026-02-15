package mateinone.bitboard

import Constants._
import java.lang.Long.numberOfTrailingZeros

object BitboardEvaluator {

  private val pawnPST = Array(
      0,   0,   0,   0,   0,   0,   0,   0,
      5,  10,  10, -20, -20,  10,  10,   5,
      5,  -5, -10,   0,   0, -10,  -5,   5,
      0,   0,   0,  20,  20,   0,   0,   0,
      5,   5,  10,  25,  25,  10,   5,   5,
     10,  10,  20,  30,  30,  20,  10,  10,
     50,  50,  50,  50,  50,  50,  50,  50,
      0,   0,   0,   0,   0,   0,   0,   0
  )

  private val knightPST = Array(
    -50, -40, -30, -30, -30, -30, -40, -50,
    -40, -20,   0,   5,   5,   0, -20, -40,
    -30,   5,  10,  15,  15,  10,   5, -30,
    -30,   0,  15,  20,  20,  15,   0, -30,
    -30,   5,  15,  20,  20,  15,   5, -30,
    -30,   0,  10,  15,  15,  10,   0, -30,
    -40, -20,   0,   0,   0,   0, -20, -40,
    -50, -40, -30, -30, -30, -30, -40, -50
  )

  private val bishopPST = Array(
    -20, -10, -10, -10, -10, -10, -10, -20,
    -10,   5,   0,   0,   0,   0,   5, -10,
    -10,  10,  10,  10,  10,  10,  10, -10,
    -10,   0,  10,  10,  10,  10,   0, -10,
    -10,   5,   5,  10,  10,   5,   5, -10,
    -10,   0,   5,  10,  10,   5,   0, -10,
    -10,   0,   0,   0,   0,   0,   0, -10,
    -20, -10, -10, -10, -10, -10, -10, -20
  )

  private val rookPST = Array(
      0,   0,   0,   5,   5,   0,   0,   0,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
      5,  10,  10,  10,  10,  10,  10,   5,
      0,   0,   0,   0,   0,   0,   0,   0
  )

  private val queenPST = Array(
    -20, -10, -10,  -5,  -5, -10, -10, -20,
    -10,   0,   5,   0,   0,   0,   0, -10,
    -10,   5,   5,   5,   5,   5,   0, -10,
      0,   0,   5,   5,   5,   5,   0,  -5,
     -5,   0,   5,   5,   5,   5,   0,  -5,
    -10,   0,   5,   5,   5,   5,   0, -10,
    -10,   0,   0,   0,   0,   0,   0, -10,
    -20, -10, -10,  -5,  -5, -10, -10, -20
  )

  private val kingMiddlePST = Array(
     20,  30,  10,   0,   0,  10,  30,  20,
     20,  20,   0,   0,   0,   0,  20,  20,
    -10, -20, -20, -20, -20, -20, -20, -10,
    -20, -30, -30, -40, -40, -30, -30, -20,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30
  )

  private val kingEndPST = Array(
    -50, -30, -30, -30, -30, -30, -30, -50,
    -30, -30,   0,   0,   0,   0, -30, -30,
    -30, -10,  20,  30,  30,  20, -10, -30,
    -30, -10,  30,  40,  40,  30, -10, -30,
    -30, -10,  30,  40,  40,  30, -10, -30,
    -30, -10,  20,  30,  30,  20, -10, -30,
    -30, -20, -10,   0,   0, -10, -20, -30,
    -50, -40, -30, -20, -20, -30, -40, -50
  )

  private val pieceValues = Array(100, 320, 330, 500, 900, 0)
  private val psts = Array(pawnPST, knightPST, bishopPST, rookPST, queenPST)

  def pieceValue(pt: Int, color: Int, sq: Int, endgame: Boolean): Int = {
    if (pt == King) {
      val pstSq = if (color == White) sq else sq ^ 56
      val kingPst = if (endgame) kingEndPST else kingMiddlePST
      kingPst(pstSq)
    } else {
      val pstSq = if (color == White) sq else sq ^ 56
      pieceValues(pt) + psts(pt)(pstSq)
    }
  }

  private def isEndgame(b: Bitboard) = {
    def inEndgame(color: Int) = b.pieceBB(color)(Queen) == 0 || (b.pieceBB(color)(Rook) == 0 && java.lang.Long.bitCount(b.pieceBB(color)(Knight) | b.pieceBB(color)(Bishop)) <= 1)
    inEndgame(White) && inEndgame(Black)
  }

  def evaluate(b: Bitboard, ply: Int): Int = {
    val endgame = isEndgame(b)
    // evalScore is from White's perspective
    var score = b.evalScore

    // King PST update for Endgame (Incremental eval assumes middle-game)
    if (endgame) {
      // Adjust King positions from Middle to End PST
      for (c <- 0 to 1) {
        val kingBB = b.pieceBB(c)(King)
        if (kingBB != 0) {
          val sideMult = if (c == White) 1 else -1
          val sq = numberOfTrailingZeros(kingBB)
          val pstSq = if (c == White) sq else sq ^ 56
          score -= sideMult * kingMiddlePST(pstSq)
          score += sideMult * kingEndPST(pstSq)
        }
      }

      val wKingBB = b.pieceBB(White)(King)
      val bKingBB = b.pieceBB(Black)(King)
      
      if (wKingBB != 0 && bKingBB != 0) {
        val wKingSq = numberOfTrailingZeros(wKingBB)
        val bKingSq = numberOfTrailingZeros(bKingBB)
        
        val wKingFile = fileOf(wKingSq); val wKingRank = rankOf(wKingSq)
        val bKingFile = fileOf(bKingSq); val bKingRank = rankOf(bKingSq)

        // 1. Push opponent king to edge
        val oppKingFile = if (b.sideToMove == White) bKingFile else wKingFile
        val oppKingRank = if (b.sideToMove == White) bKingRank else wKingRank
        val oppFileDist = Math.max(3 - oppKingFile, oppKingFile - 4)
        val oppRankDist = Math.max(3 - oppKingRank, oppKingRank - 4)
        val oppEdgeBonus = (oppFileDist + oppRankDist) * 10
        
        // 2. Reduce distance between kings
        val dist = Math.abs(wKingFile - bKingFile) + Math.abs(wKingRank - bKingRank)
        val proximityBonus = (14 - dist) * 2
        
        score += (if (b.sideToMove == White) 1 else -1) * (oppEdgeBonus + proximityBonus)
      }
    }

    if (b.sideToMove == White) score else -score
  }
}
