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
    "compareTo" in {
      _1.compare(_2) must beEqualTo(-1)
      _2.compare(_1) must beEqualTo(1)
      _1.compare(_1) must beEqualTo(0)
    }
  }

  "Square" should {
    "square" in { E3 must beEqualTo(square(E,_3)) }
    "+" in {
      A1 + (-1, 1) must beNone
      A1 + (1, -1) must beNone
      H8 + (0, 1) must beNone
      H8 + (1, 0) must beNone
      A1 + (2, 2) must beEqualTo(Some(C3))
    }
    "-" in { C3 - A1 must beEqualTo((2, 2)) }
  }

  "Board" should {

    "have the correct initial pieces" in {
      val expectedPieces: Seq[Piece] =
        Seq(
          Rook->A1, Pawn->A2, Knight->B1, Pawn->B2, Bishop->C1, Pawn->C2, Queen->D1, Pawn->D2,
          King->E1, Pawn->E2, Bishop->F1, Pawn->F2, Knight->G1, Pawn->G2, Rook->H1, Pawn->H2
        ).map { case (side, square) => Piece(White, side, square) } ++
        Seq(
          Rook->A8, Pawn->A7, Knight->B8, Pawn->B7, Bishop->C8, Pawn->C7, Queen->D8, Pawn->D7,
          King->E8, Pawn->E7, Bishop->F8, Pawn->F7, Knight->G8, Pawn->G7, Rook->H8, Pawn->H7
        ).map { case (side, square) => Piece(Black, side, square) }
      Board().pieces must containTheSameElementsAs(expectedPieces)
    }

    "have the correct initial moves for white" in moveAndCheckMoves()(A2->A3, A2->A4, B1->A3, B1->C3, B2->B3, B2->B4, C2->C3, C2->C4, D2->D3, D2->D4, E2->E3, E2->E4, F2->F3, F2->F4, G1->F3, G1->H3, G2->G3, G2->G4, H2->H3, H2->H4)
    "have the correct initial moves for black" in moveAndCheckMoves(G2->G3)(A7->A6, A7->A5, B8->A6, B8->C6, B7->B6, B7->B5, C7->C6, C7->C5, D7->D6, D7->D5, E7->E6, E7->E5, F7->F6, F7->F5, G8->F6, G8->H6, G7->G6, G7->G5, H7->H6, H7->H5)

    "be immutable" in {
      Board().move(G2->G3) must beSome.which { b =>
        b.move(G3->G4)
        onlyTheseMoved(Set(Piece(White, Pawn, G3, hasMoved = true)))(b)
      }
    }

    "1. g3" in movesAllowed(G2->G3)(Set(Piece(White, Pawn, G3, hasMoved = true)))
    "1. Nf3" in movesAllowed(G1->F3)(Set(Piece(White, Knight, F3, hasMoved = true)))
    "1. g3 a6 2. Bh3" in movesAllowed(G2->G3, A7->A6, F1->H3)(Set(Piece(White, Pawn, G3, hasMoved = true), Piece(White, Bishop, H3, hasMoved = true), Piece(Black, Pawn, A6, hasMoved = true)))
    "1. d4 e5 2. dxe5 d6 3. Bg5 dxe5 4. Bxd8" in movesAllowed(D2->D4, E7->E5, D4->E5, D7->D6, C1->G5, D6->E5, G5->D8)(Set(Piece(White, Bishop, D8, hasMoved = true), Piece(Black, Pawn, E5, hasMoved = true)), nCaptured = 3)

    // Check
    "1. e4 e5 2. f4 Bb4 d3; The last move puts White's own king in check" in lastMoveNotAllowed(E2->E4, E7->E5, F2->F4, F8->B4, D2->D3)

    // Pawn moves (two-square advance, capture, promotion, en passant)
    "white pawn to g6 after pawn to g4" in lastMoveNotAllowed(G2->G4, A7->A6, G4->G6)
    "white pawn capture non-diagonal" in lastMoveNotAllowed(D2->D4, D7->D5, D4->D5)
    "pawn promotion" in movesAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, (H7->H8 promote Queen).get)(Set(Piece(White, Queen, H8, hasMoved = true), Piece(Black, Rook, G6, hasMoved = true), Piece(Black, Pawn, E2, hasMoved = true)), nCaptured = 3)
    "pawn promotion on capture" in movesAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, (H7->G8 promote Queen).get)(Set(Piece(White, Queen, G8, hasMoved = true), Piece(Black, Rook, G6, hasMoved = true), Piece(Black, Pawn, E2, hasMoved = true)), nCaptured = 4)
    "white pawn to g8 without promotion" in lastMoveNotAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, H7->H8)
    "en passant" in movesAllowed(E2->E4, E7->E6, E4->E5, D7->D5, E5->D6)(Set(Piece(White, Pawn, D6, hasMoved = true), Piece(Black, Pawn, E6, hasMoved = true)), nCaptured = 1)
    "en passant when pawn did not advance on the last move" in lastMoveNotAllowed(E2->E4, D7->D5, E4->E5, E7->E6, E5->D6)
    "en passant when pawn advanced one move" in lastMoveNotAllowed(E2->E4, D7->D6, E4->E5, D6->D5, E5->D6)

    // Castling
    "O-O for both sides" in movesAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, `O-O`, `O-O`)(Set(Piece(White, Knight, F3, hasMoved = true),  Piece(White, Pawn, G3, hasMoved = true),  Piece(White, Bishop, H3, hasMoved = true),  Piece(White, King, G1, hasMoved = true),  Piece(White, Rook, F1, hasMoved = true),  Piece(Black, Knight, F6, hasMoved = true),  Piece(Black, Pawn, G6, hasMoved = true),  Piece(Black, Bishop, H6, hasMoved = true),  Piece(Black, King, G8, hasMoved = true),  Piece(Black, Rook, F8, hasMoved = true)))
    "O-O-O for both sides" in movesAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, `O-O-O`, `O-O-O`)(Set(Piece(White, Knight, C3, hasMoved = true),  Piece(White, Pawn, D3, hasMoved = true),  Piece(White, Bishop, G5, hasMoved = true),  Piece(White, Queen, D2, hasMoved = true),  Piece(White, Rook, D1, hasMoved = true),  Piece(White, King, C1, hasMoved = true),  Piece(Black, Knight, C6, hasMoved = true),  Piece(Black, Pawn, D6, hasMoved = true),  Piece(Black, Bishop, G4, hasMoved = true),  Piece(Black, Queen, D7, hasMoved = true),  Piece(Black, Rook, D8, hasMoved = true),  Piece(Black, King, C8, hasMoved = true)))
    "O-O for white after moving the king" in lastMoveNotAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, E1->F1, E8->F8, F1->E1, F8->E8, `O-O`)
    "O-O for black after moving the king" in lastMoveNotAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, E1->F1, E8->F8, F1->E1, F8->E8, E1->F1, `O-O`)
    "O-O for white after moving the rook" in lastMoveNotAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, H1->G1, H8->G8, G1->H1, G8->H8, `O-O`)
    "O-O for black after moving the rook" in lastMoveNotAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, H1->G1, H8->G8, G1->H1, G8->H8, H1->G1, `O-O`)
    "O-O-O for white before moving the queen" in lastMoveNotAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, `O-O-O`)
    "O-O-O for black before moving the queen" in lastMoveNotAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, `O-O-O`)
    "O-O-O for white after moving the king" in lastMoveNotAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, E1->D1, E8->D8, `O-O-O`)
    "O-O-O for black after moving the king" in lastMoveNotAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, E1->D1, E8->D8, D1->E1, `O-O-O`)
    "O-O-O for white after moving the rook" in lastMoveNotAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, A1->B1, A8->B8, B1->A1, B8->A8, `O-O-O`)
    "O-O-O for black after moving the rook" in lastMoveNotAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, A1->B1, A8->B8, B1->A1, B8->A8, A1->B1, `O-O-O`)
    "white king to g1 without castling" in lastMoveNotAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, E1->G1)
    "black king to g8 without castling" in lastMoveNotAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, `O-O`, E8->G8)
    "white king to c1 without castling" in lastMoveNotAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, E1->C1)
    "black king to c8 without castling" in lastMoveNotAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, `O-O-O`, E8->C8)

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
