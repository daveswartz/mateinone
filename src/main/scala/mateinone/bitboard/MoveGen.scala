package mateinone.bitboard

import Constants._
import java.lang.Long.numberOfTrailingZeros

case class Move(from: Int, to: Int, piece: Int, capture: Boolean, promo: Int = PieceNone, castle: Boolean = false, enPassant: Boolean = false)

object MoveGen {

  def generateMoves(b: Bitboard): List[Move] = {
    var moves = List.empty[Move]
    val color = b.sideToMove
    val oppColor = color ^ 1
    val ownOcc = b.occupancy(color)
    val oppOcc = b.occupancy(oppColor)
    val totalOcc = b.occupancy(2)
    val empty = ~totalOcc

    // 1. Pawns
    val pawns = b.pieceBB(color)(Pawn)
    if (color == White) {
      // Single push
      var singlePushes = (pawns << 8) & empty
      while (singlePushes != 0) {
        val to = numberOfTrailingZeros(singlePushes)
        val from = to - 8
        if (to >= 56) { // Promotion
          moves = generatePromotions(from, to, false, moves)
        } else {
          moves = Move(from, to, Pawn, false) :: moves
          // Double push
          if (rankOf(from) == 1 && (((1L << (from + 16)) & empty) != 0)) {
            moves = Move(from, from + 16, Pawn, false) :: moves
          }
        }
        singlePushes &= (singlePushes - 1)
      }
      // Captures
      var leftCaps = (pawns << 7) & oppOcc & ~FileH
      while (leftCaps != 0) {
        val to = numberOfTrailingZeros(leftCaps)
        val from = to - 7
        if (to >= 56) moves = generatePromotions(from, to, true, moves)
        else moves = Move(from, to, Pawn, true) :: moves
        leftCaps &= (leftCaps - 1)
      }
      var rightCaps = (pawns << 9) & oppOcc & ~FileA
      while (rightCaps != 0) {
        val to = numberOfTrailingZeros(rightCaps)
        val from = to - 9
        if (to >= 56) moves = generatePromotions(from, to, true, moves)
        else moves = Move(from, to, Pawn, true) :: moves
        rightCaps &= (rightCaps - 1)
      }
    } else { // Black pawns
      var singlePushes = (pawns >>> 8) & empty
      while (singlePushes != 0) {
        val to = numberOfTrailingZeros(singlePushes)
        val from = to + 8
        if (to <= 7) moves = generatePromotions(from, to, false, moves)
        else {
          moves = Move(from, to, Pawn, false) :: moves
          if (rankOf(from) == 6 && (((1L << (from - 16)) & empty) != 0)) {
            moves = Move(from, from - 16, Pawn, false) :: moves
          }
        }
        singlePushes &= (singlePushes - 1)
      }
      // Captures
      var leftCaps = (pawns >>> 9) & oppOcc & ~FileH
      while (leftCaps != 0) {
        val to = numberOfTrailingZeros(leftCaps)
        val from = to + 9
        if (to <= 7) moves = generatePromotions(from, to, true, moves)
        else moves = Move(from, to, Pawn, true) :: moves
        leftCaps &= (leftCaps - 1)
      }
      var rightCaps = (pawns >>> 7) & oppOcc & ~FileA
      while (rightCaps != 0) {
        val to = numberOfTrailingZeros(rightCaps)
        val from = to + 7
        if (to <= 7) moves = generatePromotions(from, to, true, moves)
        else moves = Move(from, to, Pawn, true) :: moves
        rightCaps &= (rightCaps - 1)
      }
    }

    // 2. Knights
    var knights = b.pieceBB(color)(Knight)
    while (knights != 0) {
      val from = numberOfTrailingZeros(knights)
      var atts = Attacks.KnightAttacks(from) & ~ownOcc
      while (atts != 0) {
        val to = numberOfTrailingZeros(atts)
        moves = Move(from, to, Knight, (oppOcc & (1L << to)) != 0) :: moves
        atts &= (atts - 1)
      }
      knights &= (knights - 1)
    }

    // 3. Bishops
    var bishops = b.pieceBB(color)(Bishop)
    while (bishops != 0) {
      val from = numberOfTrailingZeros(bishops)
      var atts = Attacks.bishopAttacks(from, totalOcc) & ~ownOcc
      while (atts != 0) {
        val to = numberOfTrailingZeros(atts)
        moves = Move(from, to, Bishop, (oppOcc & (1L << to)) != 0) :: moves
        atts &= (atts - 1)
      }
      bishops &= (bishops - 1)
    }

    // 4. Rooks
    var rooks = b.pieceBB(color)(Rook)
    while (rooks != 0) {
      val from = numberOfTrailingZeros(rooks)
      var atts = Attacks.rookAttacks(from, totalOcc) & ~ownOcc
      while (atts != 0) {
        val to = numberOfTrailingZeros(atts)
        moves = Move(from, to, Rook, (oppOcc & (1L << to)) != 0) :: moves
        atts &= (atts - 1)
      }
      rooks &= (rooks - 1)
    }

    // 5. Queens
    var queens = b.pieceBB(color)(Queen)
    while (queens != 0) {
      val from = numberOfTrailingZeros(queens)
      var atts = Attacks.queenAttacks(from, totalOcc) & ~ownOcc
      while (atts != 0) {
        val to = numberOfTrailingZeros(atts)
        moves = Move(from, to, Queen, (oppOcc & (1L << to)) != 0) :: moves
        atts &= (atts - 1)
      }
      queens &= (queens - 1)
    }

    // 6. King
    var kings = b.pieceBB(color)(King)
    if (kings != 0) {
      val from = numberOfTrailingZeros(kings)
      var atts = Attacks.KingAttacks(from) & ~ownOcc
      while (atts != 0) {
        val to = numberOfTrailingZeros(atts)
        moves = Move(from, to, King, (oppOcc & (1L << to)) != 0) :: moves
        atts &= (atts - 1)
      }
      
      // Castling
      if (color == White) {
        // DEBUG
        // println(s"White Castling: Rights=${b.castleRights & CastleWK}, Occ=${totalOcc & (1L << F1 | 1L << G1)}, AttackedE1=${LegalChecker.isSquareAttacked(b, E1, Black)}")
        
        if ((b.castleRights & CastleWK) != 0 && (totalOcc & (1L << F1 | 1L << G1)) == 0) {
          if (!LegalChecker.isSquareAttacked(b, E1, Black) && !LegalChecker.isSquareAttacked(b, F1, Black) && !LegalChecker.isSquareAttacked(b, G1, Black)) {
            moves = Move(E1, G1, King, false, PieceNone, true) :: moves
          }
        }
        if ((b.castleRights & CastleWQ) != 0 && (totalOcc & (1L << B1 | 1L << C1 | 1L << D1)) == 0) {
          if (!LegalChecker.isSquareAttacked(b, E1, Black) && !LegalChecker.isSquareAttacked(b, D1, Black) && !LegalChecker.isSquareAttacked(b, C1, Black)) {
            moves = Move(E1, C1, King, false, PieceNone, true) :: moves
          }
        }
      } else {
        if ((b.castleRights & CastleBK) != 0 && (totalOcc & (1L << F8 | 1L << G8)) == 0) {
          if (!LegalChecker.isSquareAttacked(b, E8, White) && !LegalChecker.isSquareAttacked(b, F8, White) && !LegalChecker.isSquareAttacked(b, G8, White)) {
            moves = Move(E8, G8, King, false, PieceNone, true) :: moves
          }
        }
        if ((b.castleRights & CastleBQ) != 0 && (totalOcc & (1L << B8 | 1L << C8 | 1L << D8)) == 0) {
          if (!LegalChecker.isSquareAttacked(b, E8, White) && !LegalChecker.isSquareAttacked(b, D8, White) && !LegalChecker.isSquareAttacked(b, C8, White)) {
            moves = Move(E8, C8, King, false, PieceNone, true) :: moves
          }
        }
      }
    }
    
    // TODO: Add En Passant (still pending)
    moves
  }

  def generateCaptures(b: Bitboard): List[Move] = {
    var moves = List.empty[Move]
    val color = b.sideToMove
    val oppColor = color ^ 1
    val oppOcc = b.occupancy(oppColor)
    val totalOcc = b.occupancy(2)

    // Simplified capture-only generation
    // Knights
    var knights = b.pieceBB(color)(Knight)
    while (knights != 0) {
      val from = numberOfTrailingZeros(knights)
      var atts = Attacks.KnightAttacks(from) & oppOcc
      while (atts != 0) {
        val to = numberOfTrailingZeros(atts)
        moves = Move(from, to, Knight, true) :: moves
        atts &= (atts - 1)
      }
      knights &= (knights - 1)
    }
    
    // Bishops/Rooks/Queens/Kings... (similar logic)
    val sliders = List((Bishop, Attacks.bishopAttacks _), (Rook, Attacks.rookAttacks _), (Queen, Attacks.queenAttacks _))
    for ((pt, func) <- sliders) {
      var bb = b.pieceBB(color)(pt)
      while (bb != 0) {
        val from = numberOfTrailingZeros(bb)
        var atts = func(from, totalOcc) & oppOcc
        while (atts != 0) {
          val to = numberOfTrailingZeros(atts)
          moves = Move(from, to, pt, true) :: moves
          atts &= (atts - 1)
        }
        bb &= (bb - 1)
      }
    }
    
    // King captures
    val kfrom = numberOfTrailingZeros(b.pieceBB(color)(King))
    var katts = Attacks.KingAttacks(kfrom) & oppOcc
    while (katts != 0) {
      val to = numberOfTrailingZeros(katts)
      moves = Move(kfrom, to, King, true) :: moves
      katts &= (katts - 1)
    }

    // Pawn captures
    val pawns = b.pieceBB(color)(Pawn)
    if (color == White) {
      var leftCaps = (pawns << 7) & oppOcc & ~FileH
      while (leftCaps != 0) {
        val to = numberOfTrailingZeros(leftCaps)
        if (to >= 56) moves = generatePromotions(to - 7, to, true, moves)
        else moves = Move(to - 7, to, Pawn, true) :: moves
        leftCaps &= (leftCaps - 1)
      }
      var rightCaps = (pawns << 9) & oppOcc & ~FileA
      while (rightCaps != 0) {
        val to = numberOfTrailingZeros(rightCaps)
        if (to >= 56) moves = generatePromotions(to - 9, to, true, moves)
        else moves = Move(to - 9, to, Pawn, true) :: moves
        rightCaps &= (rightCaps - 1)
      }
    } else {
      var leftCaps = (pawns >>> 9) & oppOcc & ~FileH
      while (leftCaps != 0) {
        val to = numberOfTrailingZeros(leftCaps)
        if (to <= 7) moves = generatePromotions(to + 9, to, true, moves)
        else moves = Move(to + 9, to, Pawn, true) :: moves
        leftCaps &= (leftCaps - 1)
      }
      var rightCaps = (pawns >>> 7) & oppOcc & ~FileA
      while (rightCaps != 0) {
        val to = numberOfTrailingZeros(rightCaps)
        if (to <= 7) moves = generatePromotions(to + 7, to, true, moves)
        else moves = Move(to + 7, to, Pawn, true) :: moves
        rightCaps &= (rightCaps - 1)
      }
    }

    moves
  }

  private def generatePromotions(from: Int, to: Int, capture: Boolean, moves: List[Move]): List[Move] = {
    Move(from, to, Pawn, capture, Queen) ::
    Move(from, to, Pawn, capture, Rook) ::
    Move(from, to, Pawn, capture, Bishop) ::
    Move(from, to, Pawn, capture, Knight) :: moves
  }
}
