package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import mateinone.bitboard.Constants._

class MateInOneSpec extends Specification {

  def isLegal(b: Bitboard, m: Move): Boolean = {
    b.makeMove(m)
    val check = LegalChecker.isInCheck(b, b.sideToMove ^ 1)
    b.unmakeMove(m)
    !check
  }

  def getLegalMoves(b: Bitboard): List[Move] = {
    MoveGen.generateMoves(b).filter(m => isLegal(b, m))
  }

  def findMove(b: Bitboard, moveStr: String): Move = {
    val moves = getLegalMoves(b)
    moves.find(m => 
      s"${Constants.squareName(m.from)}${Constants.squareName(m.to)}${if (m.promo != PieceNone) "q" else ""}" == moveStr.toLowerCase
    ).getOrElse(throw new Exception(s"Move $moveStr not found in ${moves.map(m => Constants.squareName(m.from)+Constants.squareName(m.to)).mkString(", ")}"))
  }

  "Bitboard Rules" should {

    "handle En Passant correctly" in {
      // 1. e4 e6 2. e5 d5 3. exd6
      val b = Bitboard.initial
      b.makeMove(findMove(b, "e2e4"))
      b.makeMove(findMove(b, "e7e6"))
      b.makeMove(findMove(b, "e4e5"))
      b.makeMove(findMove(b, "d7d5"))
      
      b.enPassantSq must beEqualTo(squareIndex(3, 5)) // d6
      
      val epMove = findMove(b, "e5d6")
      epMove.enPassant must beTrue
      
      b.makeMove(epMove)
      b.pieceAt(squareIndex(3, 4)) must beEqualTo(PieceNone) // d5 pawn should be gone
      b.pieceAt(squareIndex(3, 5)) must beEqualTo(Pawn) // e5 pawn now at d6
    }

    "handle Castling Rights correctly" in {
      val b = Bitboard.initial
      // Move White King
      b.makeMove(findMove(b, "e2e4"))
      b.makeMove(findMove(b, "e7e5"))
      b.makeMove(findMove(b, "e1e2"))
      
      (b.castleRights & CastleWK) must beEqualTo(0)
      (b.castleRights & CastleWQ) must beEqualTo(0)
      
      // Black should still have rights
      (b.castleRights & CastleBK) must not be equalTo(0)
    }

    "lose castling right when Rook is captured" in {
      val b = Bitboard.fromFen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
      // White Rook at a1 captures Black Rook at a8
      // Note: This is an impossible move in one go, let's use a better FEN
      val b2 = Bitboard.fromFen("r3k2r/8/8/8/8/8/R7/4K2R w Kkq - 0 1")
      // White Rook at a2 captures a8
      val m = Move(squareIndex(0, 1), squareIndex(0, 7), Rook, true)
      b2.makeMove(m)
      
      (b2.castleRights & CastleBQ) must beEqualTo(0) // Black queenside right lost
    }

    "block castling through check" in {
      // White King at e1, Black Rook at f8. Path f1 is attacked.
      val b = Bitboard.fromFen("r3k1r1/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
      // Black rook at g8 is actually attacking g1? No.
      // Setup Black Rook at f8 to attack f1
      val b2 = Bitboard.fromFen("4kr2/8/8/8/8/8/8/4K2R w K - 0 1")
      val moves = getLegalMoves(b2)
      moves.exists(_.castle) must beFalse // Cannot castle through f1
    }

    "handle Pawn Promotion" in {
      val b = Bitboard.fromFen("8/4P3/8/8/8/8/8/4K3 w - - 0 1")
      val moves = getLegalMoves(b)
      // Should have 4 promotion moves to e8
      moves.filter(_.promo != PieceNone).size must beEqualTo(4)
      
      val promoToKnight = moves.find(_.promo == Knight).get
      b.makeMove(promoToKnight)
      b.pieceAt(E8) must beEqualTo(Knight)
    }
  }

  "Bitboard Evaluation" should {
    "match original Simplified spirit" in {
      val b = Bitboard.initial
      BitboardEvaluator.evaluate(b, 0) must beEqualTo(0)
      
      b.makeMove(findMove(b, "e2e4"))
      // White pawn at e4 is better than at e2 (+20 bonus in PST)
      BitboardEvaluator.evaluate(b, 0) must beLessThan(0) // Black's turn, so negative
    }
  }

  "Search Accuracy" should {
    "find Mate in One (Fool's Mate)" in {
      val b = Bitboard.initial
      b.makeMove(findMove(b, "f2f3"))
      b.makeMove(findMove(b, "e7e5"))
      b.makeMove(findMove(b, "g2g4"))
      
      // Black to move, has mate in 1: d8h4
      val bestScore = BitboardSearch.search(b, 2, -30000, 30000, 0)
      bestScore must beGreaterThan(15000) // Black sees a winning mate
    }
  }

  "Bitboard Rules (Additional)" should {
    "detect repetition" in {
      val b = Bitboard.initial
      // 1. Nf3 Nf6 2. Ng1 Ng8
      val moves = List("g1f3", "g8f6", "f3g1", "f6g8")
      for (moveStr <- moves) b.makeMove(findMove(b, moveStr))
      // Position repeated once, need one more cycle for 3-fold
      val moves2 = List("g1f3", "g8f6", "f3g1", "f6g8")
      for (moveStr <- moves2) b.makeMove(findMove(b, moveStr))
      
      b.isThreefoldRepetition must beTrue
    }

    "detect Stalemate" in {
      // Setup stalemate position
      val b = Bitboard.initial
      val moves = List("e2e3", "a7a5", "d1h5", "a8a6", "h5a5", "h7h5", "h2h4", "a6h6", "a5c7", "f7f6", "c7d7", "e8f7", "d7b7", "d8d3", "b7b8", "d3h7", "b8c8", "f7g6", "c8e6")
      for (m <- moves) b.makeMove(findMove(b, m))
      
      getLegalMoves(b) must beEmpty
      LegalChecker.isInCheck(b, b.sideToMove) must beFalse
    }

    "detect Insufficient Material" in {
      Bitboard.fromFen("4k3/8/8/8/8/8/8/4K3 w - - 0 1").isInsufficientMaterial must beTrue
      Bitboard.fromFen("4k3/8/8/8/8/8/8/4K1N1 w - - 0 1").isInsufficientMaterial must beTrue
      Bitboard.fromFen("4k3/8/8/8/8/2B5/8/4K3 w - - 0 1").isInsufficientMaterial must beTrue
    }

    "detect Fifty-Move Rule" in {
      val b = Bitboard.initial
      b.halfMoveClock = 100
      b.isFiftyMoveRule must beTrue
    }

    "maintain evaluation score through complex sequences" in {
      val b = Bitboard.initial
      val moves = List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")
      for (m <- moves) {
        b.makeMove(findMove(b, m))
        var calculatedScore = 0
        for (c <- 0 to 1; pt <- 0 to 5) {
          val mult = if (c == White) 1 else -1
          var bb = b.pieceBB(c)(pt)
          while (bb != 0) {
            val sq = java.lang.Long.numberOfTrailingZeros(bb)
            calculatedScore += mult * BitboardEvaluator.pieceValue(pt, c, sq, false)
            bb &= (bb - 1)
          }
        }
        b.evalScore must beEqualTo(calculatedScore)
      }
      success
    }
  }
}
