package mateinone.bitboard

import Constants._
import java.lang.Long.numberOfTrailingZeros
import scala.collection.mutable.ArrayBuffer

object MoveGen {

  def generateMoves(b: Bitboard): Array[Int] = {
    val moves = new ArrayBuffer[Int](64)
    val color = b.sideToMove
    val oppColor = color ^ 1
    val ownOcc = b.occupancy(color)
    val oppOcc = b.occupancy(oppColor)
    val totalOcc = b.occupancy(2)
    val empty = ~totalOcc

    // 1. Pawns
    val pawns = b.pieceBB(color)(Pawn)
    if (color == White) {
      var singlePushes = (pawns << 8) & empty
      while (singlePushes != 0) {
        val to = numberOfTrailingZeros(singlePushes)
        val from = to - 8
        if (to >= 56) generatePromotions(from, to, false, moves)
        else {
          moves += packMove(from, to, Pawn, PieceNone, false, false, false)
          if (rankOf(from) == 1 && (((1L << (from + 16)) & empty) != 0)) {
            moves += packMove(from, from + 16, Pawn, PieceNone, false, false, false)
          }
        }
        singlePushes &= (singlePushes - 1)
      }
      var leftCaps = (pawns << 7) & oppOcc & ~FileH
      while (leftCaps != 0) {
        val to = numberOfTrailingZeros(leftCaps)
        val from = to - 7
        if (to >= 56) generatePromotions(from, to, true, moves)
        else moves += packMove(from, to, Pawn, PieceNone, true, false, false)
        leftCaps &= (leftCaps - 1)
      }
      var rightCaps = (pawns << 9) & oppOcc & ~FileA
      while (rightCaps != 0) {
        val to = numberOfTrailingZeros(rightCaps)
        val from = to - 9
        if (to >= 56) generatePromotions(from, to, true, moves)
        else moves += packMove(from, to, Pawn, PieceNone, true, false, false)
        rightCaps &= (rightCaps - 1)
      }
    } else {
      var singlePushes = (pawns >>> 8) & empty
      while (singlePushes != 0) {
        val to = numberOfTrailingZeros(singlePushes)
        val from = to + 8
        if (to <= 7) generatePromotions(from, to, false, moves)
        else {
          moves += packMove(from, to, Pawn, PieceNone, false, false, false)
          if (rankOf(from) == 6 && (((1L << (from - 16)) & empty) != 0)) {
            moves += packMove(from, from - 16, Pawn, PieceNone, false, false, false)
          }
        }
        singlePushes &= (singlePushes - 1)
      }
      var leftCaps = (pawns >>> 9) & oppOcc & ~FileH
      while (leftCaps != 0) {
        val to = numberOfTrailingZeros(leftCaps)
        val from = to + 9
        if (to <= 7) generatePromotions(from, to, true, moves)
        else moves += packMove(from, to, Pawn, PieceNone, true, false, false)
        leftCaps &= (leftCaps - 1)
      }
      var rightCaps = (pawns >>> 7) & oppOcc & ~FileA
      while (rightCaps != 0) {
        val to = numberOfTrailingZeros(rightCaps)
        val from = to + 7
        if (to <= 7) generatePromotions(from, to, true, moves)
        else moves += packMove(from, to, Pawn, PieceNone, true, false, false)
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
        moves += packMove(from, to, Knight, PieceNone, (oppOcc & (1L << to)) != 0, false, false)
        atts &= (atts - 1)
      }
      knights &= (knights - 1)
    }

    // 3. Sliders
    val sliderTypes = Array(Bishop, Rook, Queen)
    val sliderFuncs = Array(Attacks.bishopAttacks _, Attacks.rookAttacks _, Attacks.queenAttacks _)
    for (i <- 0 until 3) {
      var bb = b.pieceBB(color)(sliderTypes(i))
      while (bb != 0) {
        val from = numberOfTrailingZeros(bb)
        var atts = sliderFuncs(i)(from, totalOcc) & ~ownOcc
        while (atts != 0) {
          val to = numberOfTrailingZeros(atts)
          moves += packMove(from, to, sliderTypes(i), PieceNone, (oppOcc & (1L << to)) != 0, false, false)
          atts &= (atts - 1)
        }
        bb &= (bb - 1)
      }
    }

    // 4. King
    var kings = b.pieceBB(color)(King)
    if (kings != 0) {
      val from = numberOfTrailingZeros(kings)
      var atts = Attacks.KingAttacks(from) & ~ownOcc
      while (atts != 0) {
        val to = numberOfTrailingZeros(atts)
        moves += packMove(from, to, King, PieceNone, (oppOcc & (1L << to)) != 0, false, false)
        atts &= (atts - 1)
      }
      
      if (color == White) {
        if ((b.castleRights & CastleWK) != 0 && (totalOcc & (1L << F1 | 1L << G1)) == 0) {
          if (!LegalChecker.isSquareAttacked(b, E1, Black) && !LegalChecker.isSquareAttacked(b, F1, Black) && !LegalChecker.isSquareAttacked(b, G1, Black)) {
            moves += packMove(E1, G1, King, PieceNone, false, true, false)
          }
        }
        if ((b.castleRights & CastleWQ) != 0 && (totalOcc & (1L << B1 | 1L << C1 | 1L << D1)) == 0) {
          if (!LegalChecker.isSquareAttacked(b, E1, Black) && !LegalChecker.isSquareAttacked(b, D1, Black) && !LegalChecker.isSquareAttacked(b, C1, Black)) {
            moves += packMove(E1, C1, King, PieceNone, false, true, false)
          }
        }
      } else {
        if ((b.castleRights & CastleBK) != 0 && (totalOcc & (1L << F8 | 1L << G8)) == 0) {
          if (!LegalChecker.isSquareAttacked(b, E8, White) && !LegalChecker.isSquareAttacked(b, F8, White) && !LegalChecker.isSquareAttacked(b, G8, White)) {
            moves += packMove(E8, G8, King, PieceNone, false, true, false)
          }
        }
        if ((b.castleRights & CastleBQ) != 0 && (totalOcc & (1L << B8 | 1L << C8 | 1L << D8)) == 0) {
          if (!LegalChecker.isSquareAttacked(b, E8, White) && !LegalChecker.isSquareAttacked(b, D8, White) && !LegalChecker.isSquareAttacked(b, C8, White)) {
            moves += packMove(E8, C8, King, PieceNone, false, true, false)
          }
        }
      }
    }
    
    // 5. EP
    if (b.enPassantSq != SquareNone) {
      val target = b.enPassantSq
      val attackers = b.pieceBB(color)(Pawn)
      if (color == White) {
        if (fileOf(target) > 0 && (attackers & (1L << (target - 9))) != 0) moves += packMove(target - 9, target, Pawn, PieceNone, true, false, true)
        if (fileOf(target) < 7 && (attackers & (1L << (target - 7))) != 0) moves += packMove(target - 7, target, Pawn, PieceNone, true, false, true)
      } else {
        if (fileOf(target) > 0 && (attackers & (1L << (target + 7))) != 0) moves += packMove(target + 7, target, Pawn, PieceNone, true, false, true)
        if (fileOf(target) < 7 && (attackers & (1L << (target + 9))) != 0) moves += packMove(target + 9, target, Pawn, PieceNone, true, false, true)
      }
    }
    
    moves.toArray
  }

  def generateCaptures(b: Bitboard): Array[Int] = {
    val moves = new ArrayBuffer[Int](16)
    val color = b.sideToMove
    val oppColor = color ^ 1
    val oppOcc = b.occupancy(oppColor)
    val totalOcc = b.occupancy(2)

    var knights = b.pieceBB(color)(Knight)
    while (knights != 0) {
      val from = numberOfTrailingZeros(knights)
      var atts = Attacks.KnightAttacks(from) & oppOcc
      while (atts != 0) {
        val to = numberOfTrailingZeros(atts)
        moves += packMove(from, to, Knight, PieceNone, true, false, false)
        atts &= (atts - 1)
      }
      knights &= (knights - 1)
    }
    
    val sliderTypes = Array(Bishop, Rook, Queen)
    val sliderFuncs = Array(Attacks.bishopAttacks _, Attacks.rookAttacks _, Attacks.queenAttacks _)
    for (i <- 0 until 3) {
      var bb = b.pieceBB(color)(sliderTypes(i))
      while (bb != 0) {
        val from = numberOfTrailingZeros(bb)
        var atts = sliderFuncs(i)(from, totalOcc) & oppOcc
        while (atts != 0) {
          val to = numberOfTrailingZeros(atts)
          moves += packMove(from, to, sliderTypes(i), PieceNone, true, false, false)
          atts &= (atts - 1)
        }
        bb &= (bb - 1)
      }
    }
    
    val kfrom = numberOfTrailingZeros(b.pieceBB(color)(King))
    var katts = Attacks.KingAttacks(kfrom) & oppOcc
    while (katts != 0) {
      val to = numberOfTrailingZeros(katts)
      moves += packMove(kfrom, to, King, PieceNone, true, false, false)
      katts &= (katts - 1)
    }

    val pawns = b.pieceBB(color)(Pawn)
    if (color == White) {
      var leftCaps = (pawns << 7) & oppOcc & ~FileH
      while (leftCaps != 0) {
        val to = numberOfTrailingZeros(leftCaps)
        if (to >= 56) generatePromotions(to - 7, to, true, moves)
        else moves += packMove(to - 7, to, Pawn, PieceNone, true, false, false)
        leftCaps &= (leftCaps - 1)
      }
      var rightCaps = (pawns << 9) & oppOcc & ~FileA
      while (rightCaps != 0) {
        val to = numberOfTrailingZeros(rightCaps)
        if (to >= 56) generatePromotions(to - 9, to, true, moves)
        else moves += packMove(to - 9, to, Pawn, PieceNone, true, false, false)
        rightCaps &= (rightCaps - 1)
      }
    } else {
      var leftCaps = (pawns >>> 9) & oppOcc & ~FileH
      while (leftCaps != 0) {
        val to = numberOfTrailingZeros(leftCaps)
        if (to <= 7) generatePromotions(to + 9, to, true, moves)
        else moves += packMove(to + 9, to, Pawn, PieceNone, true, false, false)
        leftCaps &= (leftCaps - 1)
      }
      var rightCaps = (pawns >>> 7) & oppOcc & ~FileA
      while (rightCaps != 0) {
        val to = numberOfTrailingZeros(rightCaps)
        if (to <= 7) generatePromotions(to + 7, to, true, moves)
        else moves += packMove(to + 7, to, Pawn, PieceNone, true, false, false)
        rightCaps &= (rightCaps - 1)
      }
    }

    if (b.enPassantSq != SquareNone) {
      val target = b.enPassantSq
      val attackers = b.pieceBB(color)(Pawn)
      if (color == White) {
        if (fileOf(target) > 0 && (attackers & (1L << (target - 9))) != 0) moves += packMove(target - 9, target, Pawn, PieceNone, true, false, true)
        if (fileOf(target) < 7 && (attackers & (1L << (target - 7))) != 0) moves += packMove(target - 7, target, Pawn, PieceNone, true, false, true)
      } else {
        if (fileOf(target) > 0 && (attackers & (1L << (target + 7))) != 0) moves += packMove(target + 7, target, Pawn, PieceNone, true, false, true)
        if (fileOf(target) < 7 && (attackers & (1L << (target + 9))) != 0) moves += packMove(target + 9, target, Pawn, PieceNone, true, false, true)
      }
    }

    moves.toArray
  }

  private def generatePromotions(from: Int, to: Int, capture: Boolean, moves: ArrayBuffer[Int]): Unit = {
    moves += packMove(from, to, Pawn, Queen, capture, false, false)
    moves += packMove(from, to, Pawn, Rook, capture, false, false)
    moves += packMove(from, to, Pawn, Bishop, capture, false, false)
    moves += packMove(from, to, Pawn, Knight, capture, false, false)
  }
}
