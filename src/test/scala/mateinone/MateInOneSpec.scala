package mateinone

import org.specs2.mutable._
import Square._
import File._
import Rank._
import SimpleMove._
import Castle._
import Move._
import org.specs2.execute.Result

class MateInOneSpec extends Specification {

  "File" should {
    "+" in {
      A + -1 must beNone
      H + 1 must beNone
      A + 2 must beEqualTo(Some(C))
    }
    "-" in { H - A must beEqualTo(7) }
    "toString" in { B.toString must beEqualTo("b") }
    "compareTo" in {
      A.compare(B) must beEqualTo(-1)
      B.compare(A) must beEqualTo(1)
      A.compare(A) must beEqualTo(0)
    }
  }

  "Rank" should {
    "+" in {
      _1 + -1 must beNone
      _8 + 1 must beNone
      _1 + 2 must beEqualTo(Some(_3))
    }
    "-" in { _8 - _1 must beEqualTo(7) }
    "toString" in { _2.toString must beEqualTo("2") }
    "compareTo" in {
      _1.compare(_2) must beEqualTo(-1)
      _2.compare(_1) must beEqualTo(1)
      _1.compare(_1) must beEqualTo(0)
    }
  }

  "Square" should {
    "square" in { e3 must beEqualTo(square(E,_3)) }
    "+" in {
      a1 + (-1, 1) must beNone
      a1 + (1, -1) must beNone
      h8 + (0, 1) must beNone
      h8 + (1, 0) must beNone
      a1 + (2, 2) must beEqualTo(Some(c3))
    }
    "-" in { c3 - a1 must beEqualTo((2, 2)) }
    "toString" in { b5.toString must beEqualTo("b5") }
  }

  "Board" should {

    "have the correct initial pieces" in {
      val expectedPieces: Seq[Piece] =
        Seq(
          Rook->a1, Pawn->a2, Knight->b1, Pawn->b2, Bishop->c1, Pawn->c2, Queen->d1, Pawn->d2,
          King->e1, Pawn->e2, Bishop->f1, Pawn->f2, Knight->g1, Pawn->g2, Rook->h1, Pawn->h2
        ).map { case (side, square) => Piece(White, side, square) } ++
        Seq(
          Rook->a8, Pawn->a7, Knight->b8, Pawn->b7, Bishop->c8, Pawn->c7, Queen->d8, Pawn->d7,
          King->e8, Pawn->e7, Bishop->f8, Pawn->f7, Knight->g8, Pawn->g7, Rook->h8, Pawn->h7
        ).map { case (side, square) => Piece(Black, side, square) }
      Board().pieces must containTheSameElementsAs(expectedPieces)
    }

    "have the correct initial moves for white" in moveAndCheckMoves()(a2->a3, a2->a4, b1->a3, b1->c3, b2->b3, b2->b4, c2->c3, c2->c4, d2->d3, d2->d4, e2->e3, e2->e4, f2->f3, f2->f4, g1->f3, g1->h3, g2->g3, g2->g4, h2->h3, h2->h4)
    "have the correct initial moves for black" in moveAndCheckMoves(g2->g3)(a7->a6, a7->a5, b8->a6, b8->c6, b7->b6, b7->b5, c7->c6, c7->c5, d7->d6, d7->d5, e7->e6, e7->e5, f7->f6, f7->f5, g8->f6, g8->h6, g7->g6, g7->g5, h7->h6, h7->h5)

    "be immutable" in {
      Board().move(g2->g3) must beSome.which { b =>
        b.move(g3->g4)
        onlyTheseMoved(Set(Piece(White, Pawn, g3, hasMoved = true)))(b)
      }
    }

    "1. g3" in movesAllowed(g2->g3)(Set(Piece(White, Pawn, g3, hasMoved = true)))
    "1. Nf3" in movesAllowed(g1->f3)(Set(Piece(White, Knight, f3, hasMoved = true)))
    "1. g3 a6 2. Bh3" in movesAllowed(g2->g3, a7->a6, f1->h3)(Set(Piece(White, Pawn, g3, hasMoved = true), Piece(White, Bishop, h3, hasMoved = true), Piece(Black, Pawn, a6, hasMoved = true)))
    "1. d4 e5 2. dxe5 d6 3. Bg5 dxe5 4. Bxd8" in movesAllowed(d2->d4, e7->e5, d4->e5, d7->d6, c1->g5, d6->e5, g5->d8)(Set(Piece(White, Bishop, d8, hasMoved = true), Piece(Black, Pawn, e5, hasMoved = true)), nCaptured = 3)

    // Check
    "1. e4 e5 2. f4 Bb4 d3; The last move puts White's own king in check" in lastMoveNotAllowed(e2->e4, e7->e5, f2->f4, f8->b4, d2->d3)

    // Pawn moves (two-square advance, capture, promotion, en passant)
    "white pawn to g6 after pawn to g4" in lastMoveNotAllowed(g2->g4, a7->a6, g4->g6)
    "white pawn capture non-diagonal" in lastMoveNotAllowed(d2->d4, d7->d5, d4->d5)
    "pawn promotion" in movesAllowed(g2->g4, h7->h5, h2->h4, h5->g4, h4->h5, h8->h6, f2->f3, h6->g6, h5->h6, g4->f3, h6->h7, f3->e2, (h7->h8 promote Queen).get)(Set(Piece(White, Queen, h8, hasMoved = true), Piece(Black, Rook, g6, hasMoved = true), Piece(Black, Pawn, e2, hasMoved = true)), nCaptured = 3)
    "pawn promotion on capture" in movesAllowed(g2->g4, h7->h5, h2->h4, h5->g4, h4->h5, h8->h6, f2->f3, h6->g6, h5->h6, g4->f3, h6->h7, f3->e2, (h7->g8 promote Queen).get)(Set(Piece(White, Queen, g8, hasMoved = true), Piece(Black, Rook, g6, hasMoved = true), Piece(Black, Pawn, e2, hasMoved = true)), nCaptured = 4)
    "white pawn to g8 without promotion" in lastMoveNotAllowed(g2->g4, h7->h5, h2->h4, h5->g4, h4->h5, h8->h6, f2->f3, h6->g6, h5->h6, g4->f3, h6->h7, f3->e2, h7->h8)
    "en passant" in movesAllowed(e2->e4, e7->e6, e4->e5, d7->d5, e5->d6)(Set(Piece(White, Pawn, d6, hasMoved = true), Piece(Black, Pawn, e6, hasMoved = true)), nCaptured = 1)
    "en passant when pawn did not advance on the last move" in lastMoveNotAllowed(e2->e4, d7->d5, e4->e5, e7->e6, e5->d6)
    "en passant when pawn advanced one move" in lastMoveNotAllowed(e2->e4, d7->d6, e4->e5, d6->d5, e5->d6)

    // Castling
    "O-O for both sides" in movesAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, `O-O`)(Set(Piece(White, Knight, f3, hasMoved = true),  Piece(White, Pawn, g3, hasMoved = true),  Piece(White, Bishop, h3, hasMoved = true),  Piece(White, King, g1, hasMoved = true),  Piece(White, Rook, f1, hasMoved = true),  Piece(Black, Knight, f6, hasMoved = true),  Piece(Black, Pawn, g6, hasMoved = true),  Piece(Black, Bishop, h6, hasMoved = true),  Piece(Black, King, g8, hasMoved = true),  Piece(Black, Rook, f8, hasMoved = true)))
    "O-O-O for both sides" in movesAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, `O-O-O`, `O-O-O`)(Set(Piece(White, Knight, c3, hasMoved = true),  Piece(White, Pawn, d3, hasMoved = true),  Piece(White, Bishop, g5, hasMoved = true),  Piece(White, Queen, d2, hasMoved = true),  Piece(White, Rook, d1, hasMoved = true),  Piece(White, King, c1, hasMoved = true),  Piece(Black, Knight, c6, hasMoved = true),  Piece(Black, Pawn, d6, hasMoved = true),  Piece(Black, Bishop, g4, hasMoved = true),  Piece(Black, Queen, d7, hasMoved = true),  Piece(Black, Rook, d8, hasMoved = true),  Piece(Black, King, c8, hasMoved = true)))
    "O-O for white after moving the king" in lastMoveNotAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, e1->f1, e8->f8, f1->e1, f8->e8, `O-O`)
    "O-O for black after moving the king" in lastMoveNotAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, e1->f1, e8->f8, f1->e1, f8->e8, e1->f1, `O-O`)
    "O-O for white after moving the rook" in lastMoveNotAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, h1->g1, h8->g8, g1->h1, g8->h8, `O-O`)
    "O-O for black after moving the rook" in lastMoveNotAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, h1->g1, h8->g8, g1->h1, g8->h8, h1->g1, `O-O`)
    "O-O-O for white before moving the queen" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, `O-O-O`)
    "O-O-O for black before moving the queen" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, `O-O-O`)
    "O-O-O for white after moving the king" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, e1->d1, e8->d8, `O-O-O`)
    "O-O-O for black after moving the king" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, e1->d1, e8->d8, d1->e1, `O-O-O`)
    "O-O-O for white after moving the rook" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, a1->b1, a8->b8, b1->a1, b8->a8, `O-O-O`)
    "O-O-O for black after moving the rook" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, a1->b1, a8->b8, b1->a1, b8->a8, a1->b1, `O-O-O`)
    "white king to g1 without castling" in lastMoveNotAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, e1->g1)
    "black king to g8 without castling" in lastMoveNotAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, e8->g8)
    "white king to c1 without castling" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, e1->c1)
    "black king to c8 without castling" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, `O-O-O`, e8->c8)

  }

  // Checks each move is generated and allowed
  def movesAllowed(moves: Either[Move, Side => Move]*)(movedPieces: Set[Piece] = Set(), nCaptured: Int = 0) = {
    def recur(board: Option[Board], remaining: List[Either[Move, Side => Move]]): Result = {
      (board, remaining) match {
        case (Some(b), head :: tail) =>
          b.moves must contain(toMove(head, b.turn))
          recur(b.move(head), tail)
        case (Some(b), Nil) =>
          onlyTheseMoved(movedPieces, nCaptured)(b)
        case (None, _) =>
          failure
      }
    }
    recur(Some(Board()), moves.toList)
  }

  // Checks each move except the last is generated and allowed
  def lastMoveNotAllowed(moves: Either[Move, Side => Move]*) = {
    def recur(board: Option[Board], remaining: List[Either[Move, Side => Move]]): Result = {
      (board, remaining) match {
        case (Some(b), last :: Nil) =>
          b.moves must not contain(toMove(last, b.turn))
          recur(b.move(last), Nil)
        case (Some(b), head :: tail) =>
          b.moves must contain(toMove(head, b.turn))
          recur(b.move(head), tail)
        case (Some(b), Nil) =>
          failure
        case (None, head :: tail) =>
          failure
        case (None, Nil) =>
          success
      }
    }
    recur(Some(Board()), moves.toList)
  }

  // Only checks the number of captures as the actual pieces captured are determined by the initial board pieces and the moved pieces specified
  def onlyTheseMoved(movedPieces: Set[Piece] = Set(), nCaptured: Int = 0): Board => Result = {
    val initialPieces = Board().pieces
    (board) => {
      val stationaryPieces = board.pieces.filterNot(_.hasMoved)
      stationaryPieces.size + movedPieces.size + nCaptured must beEqualTo(32)
      initialPieces must containAllOf(stationaryPieces.toSeq)
      board.pieces must containAllOf(movedPieces.toSeq)
    }
  }

  def moveAndCheckMoves(moves: Either[Move, Side => Move]*)(expectedMoves: Move*) =
    Board().move(moves :_*) must beSome.which(_.moves must containTheSameElementsAs(expectedMoves))

}
