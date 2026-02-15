package mateinone.bitboard

import Constants._
import java.lang.Long.numberOfTrailingZeros

object LegalChecker {

  def isSquareAttacked(b: Bitboard, sq: Int, attackerColor: Int): Boolean = {
    val totalOcc = b.occupancy(2)
    val oppColor = attackerColor
    
    // 1. Attack by Pawns
    // If we are at sq, and a pawn of attackerColor is at sq - offset, it attacks us.
    // White Pawns attack North-East (+9) and North-West (+7)
    // Black Pawns attack South-East (-7) and South-West (-9)
    if (attackerColor == White) {
      val attackers = b.pieceBB(White)(Pawn)
      // sq was attacked by a white pawn if white pawn is at sq-7 or sq-9
      if (sq >= 7 && (attackers & (1L << (sq - 7))) != 0 && fileOf(sq) != 7) return true
      if (sq >= 9 && (attackers & (1L << (sq - 9))) != 0 && fileOf(sq) != 0) return true
    } else {
      val attackers = b.pieceBB(Black)(Pawn)
      if (sq <= 56 && (attackers & (1L << (sq + 7))) != 0 && fileOf(sq) != 0) return true
      if (sq <= 54 && (attackers & (1L << (sq + 9))) != 0 && fileOf(sq) != 7) return true
    }

    // 2. Attack by Knights
    if ((Attacks.KnightAttacks(sq) & b.pieceBB(oppColor)(Knight)) != 0) return true

    // 3. Attack by Kings
    if ((Attacks.KingAttacks(sq) & b.pieceBB(oppColor)(King)) != 0) return true

    // 4. Sliding: Bishops/Queens
    val bishopQueens = b.pieceBB(oppColor)(Bishop) | b.pieceBB(oppColor)(Queen)
    if ((Attacks.bishopAttacks(sq, totalOcc) & bishopQueens) != 0) return true

    // 5. Sliding: Rooks/Queens
    val rookQueens = b.pieceBB(oppColor)(Rook) | b.pieceBB(oppColor)(Queen)
    if ((Attacks.rookAttacks(sq, totalOcc) & rookQueens) != 0) return true

    false
  }

  def isInCheck(b: Bitboard, color: Int): Boolean = {
    val kingBB = b.pieceBB(color)(King)
    if (kingBB == 0) return false // Should not happen in real games
    val kingSq = numberOfTrailingZeros(kingBB)
    isSquareAttacked(b, kingSq, color ^ 1)
  }
}
