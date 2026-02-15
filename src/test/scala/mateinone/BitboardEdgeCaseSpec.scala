package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import mateinone.bitboard.Constants._

class BitboardEdgeCaseSpec extends Specification {

  def isLegal(b: Bitboard, m: Move): Boolean = {
    b.makeMove(m)
    val check = LegalChecker.isInCheck(b, b.sideToMove ^ 1)
    b.unmakeMove(m)
    !check
  }

  def findMove(b: Bitboard, moveStr: String): Move = {
    val moves = MoveGen.generateMoves(b).filter(m => isLegal(b, m))
    moves.find(m => 
      s"${Constants.squareName(m.from)}${Constants.squareName(m.to)}" == moveStr.toLowerCase
    ).getOrElse(throw new Exception(s"Move $moveStr not found"))
  }

  "Bitboard State Integrity" should {
    "maintain perfect hash and eval sync through a sequence" in {
      val b = Bitboard.initial
      val moves = List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")
      
      for (moveStr <- moves) {
        val m = findMove(b, moveStr)
        b.makeMove(m)
        
        // Verify incremental eval Score (White's perspective)
        val calculatedEval = BitboardEvaluator.calculateFullScore(b)
        b.evalScore must beEqualTo(calculatedEval)
        
        // Verify incremental Hash
        val freshHashBoard = Bitboard.fromFen(FENUtils.toFen(b))
        b.hash must beEqualTo(freshHashBoard.hash)
      }
      success
    }
  }

  "Move Legality Edge Cases" should {
    "detect illegal En Passant (Discovered Check on Rank)" in {
      // White king at a5, black rook at h5. Pawns at d5 and e5.
      // Black just played e7e5. White's capture dxe6 would reveal check.
      val b = Bitboard.fromFen("8/8/8/K2Pp2r/8/8/8/4k3 w - e6 0 1")
      val moves = MoveGen.generateMoves(b)
      val epMove = moves.find(_.enPassant)
      
      epMove must beSome
      isLegal(b, epMove.get) must beFalse
    }

    "block castling through check" in {
      // White king e1, Rook h1. Black rook at f8 attacks f1.
      val b = Bitboard.fromFen("4kr2/8/8/8/8/8/8/4K2R w K - 0 1")
      val moves = MoveGen.generateMoves(b)
      moves.exists(_.castle) must beFalse
    }

    "allow castling when rook is attacked but king path is safe" in {
      // White king e1, Rook h1. Black rook at h8 attacks h1.
      val b = Bitboard.fromFen("7r/8/8/8/8/8/8/4K2R w K - 0 1")
      val moves = MoveGen.generateMoves(b)
      moves.exists(_.castle) must beTrue
    }
  }
}

object FENUtils {
  def toFen(b: Bitboard): String = {
    val sb = new StringBuilder()
    for (r <- 7 to 0 by -1) {
      var empty = 0
      for (f <- 0 to 7) {
        val sq = Constants.squareIndex(f, r)
        val p = b.pieceAt(sq)
        val c = b.colorAt(sq)
        if (c == Constants.ColorNone) {
          empty += 1
        } else {
          if (empty > 0) { sb.append(empty); empty = 0 }
          val char = p match {
            case Constants.Pawn => 'p'
            case Constants.Knight => 'n'
            case Constants.Bishop => 'b'
            case Constants.Rook => 'r'
            case Constants.Queen => 'q'
            case Constants.King => 'k'
          }
          sb.append(if (c == Constants.White) char.toUpper else char)
        }
      }
      if (empty > 0) sb.append(empty)
      if (r > 0) sb.append("/")
    }
    sb.append(if (b.sideToMove == Constants.White) " w " else " b ")
    
    // Castling
    if (b.castleRights == 0) sb.append("-")
    else {
      if ((b.castleRights & Constants.CastleWK) != 0) sb.append("K")
      if ((b.castleRights & Constants.CastleWQ) != 0) sb.append("Q")
      if ((b.castleRights & Constants.CastleBK) != 0) sb.append("k")
      if ((b.castleRights & Constants.CastleBQ) != 0) sb.append("q")
    }
    
    sb.append(" ")
    if (b.enPassantSq == Constants.SquareNone) sb.append("-")
    else sb.append(Constants.squareName(b.enPassantSq))
    
    sb.append(s" ${b.halfMoveClock} ${b.fullMoveNumber}")
    sb.toString()
  }
}
