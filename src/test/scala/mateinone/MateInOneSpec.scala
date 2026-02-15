package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import mateinone.bitboard.Constants._

class MateInOneSpec extends Specification {

  "Bitboard Engine" should {

    "initialize correctly" in {
      val b = Bitboard.initial
      b.pieceBB(White)(Pawn) must not be equalTo(0L)
      b.pieceBB(Black)(King) must not be equalTo(0L)
      b.sideToMove must beEqualTo(White)
    }

    "generate opening moves" in {
      val b = Bitboard.initial
      val moves = MoveGen.generateMoves(b)
      // 16 pawn moves + 4 knight moves = 20
      moves.size must beEqualTo(20)
    }

    "handle simple moves" in {
      val b = Bitboard.initial
      // e2e4
      val e2e4 = MoveGen.generateMoves(b).find(m => m.from == squareIndex(4, 1) && m.to == squareIndex(4, 3)).get
      b.makeMove(e2e4)
      b.pieceAt(squareIndex(4, 3)) must beEqualTo(Pawn)
      b.sideToMove must beEqualTo(Black)
    }

    "detect checkmate (Scholar's Mate)" in {
      val b = Bitboard.initial
      // 1. e4 e5 2. Qh5 Nc6 3. Bc4 Nf6 4. Qxf7#
      val moves = List(
        "e2e4", "e7e5",
        "d1h5", "b8c6",
        "f1c4", "g8f6",
        "h5f7"
      )
      
      for (moveStr <- moves) {
        val legalMoves = MoveGen.generateMoves(b)
        val m = legalMoves.find(lm => 
          Constants.squareName(lm.from) + Constants.squareName(lm.to) == moveStr
        ).get
        b.makeMove(m)
      }
      
      val nextMoves = MoveGen.generateMoves(b)
      val legalNextMoves = nextMoves.filter(m => {
        b.makeMove(m)
        val legal = !LegalChecker.isInCheck(b, b.sideToMove ^ 1)
        b.unmakeMove(m)
        legal
      })
      
      legalNextMoves must beEmpty
      LegalChecker.isInCheck(b, b.sideToMove) must beTrue
    }

    "detect repetition" in {
      val b = Bitboard.initial
      // 1. Nf3 Nf6 2. Ng1 Ng8
      val moves = List("g1f3", "g8f6", "f3g1", "f6g8")
      for (moveStr <- moves) {
        val legalMoves = MoveGen.generateMoves(b)
        val m = legalMoves.find(lm => 
          Constants.squareName(lm.from) + Constants.squareName(lm.to) == moveStr
        ).get
        b.makeMove(m)
      }
      // Position repeated once, need one more cycle for 3-fold
      val moves2 = List("g1f3", "g8f6", "f3g1", "f6g8")
      for (moveStr <- moves2) {
        val legalMoves = MoveGen.generateMoves(b)
        val m = legalMoves.find(lm => 
          Constants.squareName(lm.from) + Constants.squareName(lm.to) == moveStr
        ).get
        b.makeMove(m)
      }
      
      b.isThreefoldRepetition must beTrue
    }
  }

  "Bitboard Search" should {
    "find mate in one" in {
      val b = Bitboard.initial
      // Setup Scholar's mate pos: e4 e5 Qh5 Nc6 Bc4 Nf6
      val setup = List("e2e4", "e7e5", "d1h5", "b8c6", "f1c4", "g8f6")
      for (moveStr <- setup) {
        val m = MoveGen.generateMoves(b).find(lm => 
          Constants.squareName(lm.from) + Constants.squareName(lm.to) == moveStr
        ).get
        b.makeMove(m)
      }
      
      val bestScore = BitboardSearch.search(b, 2, -30000, 30000, 0)
      bestScore must beGreaterThan(15000) // Mate score
    }
  }
}
