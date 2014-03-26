package mateinone

import org.specs2.mutable._
import Square._
import File._
import Rank._
import MoveImplicits._
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
        ).map { case (side, square) => Piece(White, side, square, hasMoved = false) } ++
        Seq(
          Rook->A8, Pawn->A7, Knight->B8, Pawn->B7, Bishop->C8, Pawn->C7, Queen->D8, Pawn->D7,
          King->E8, Pawn->E7, Bishop->F8, Pawn->F7, Knight->G8, Pawn->G7, Rook->H8, Pawn->H7
        ).map { case (side, square) => Piece(Black, side, square, hasMoved = false) }
      Board.initial.pieces must containTheSameElementsAs(expectedPieces)
    }
    "have the correct initial moves for white" in moveAndCheckMoves()(A2->A3, A2->A4, B1->A3, B1->C3, B2->B3, B2->B4, C2->C3, C2->C4, D2->D3, D2->D4, E2->E3, E2->E4, F2->F3, F2->F4, G1->F3, G1->H3, G2->G3, G2->G4, H2->H3, H2->H4)
    "have the correct initial moves for black" in moveAndCheckMoves(G2->G3)(A7->A6, A7->A5, B8->A6, B8->C6, B7->B6, B7->B5, C7->C6, C7->C5, D7->D6, D7->D5, E7->E6, E7->E5, F7->F6, F7->F5, G8->F6, G8->H6, G7->G6, G7->G5, H7->H6, H7->H5)
    "be immutable" in {
      Board.initial.move(G2->G3) must beSome.which { b =>
        b.move(G3->G4)
        onlyTheseMoved(Set(Piece(White, Pawn, G3, _)))(b)
      }
    }
  }

  "Simple moves" should {
    "1. g3"                                   in movesAllowed(G2->G3)(Set(Piece(White, Pawn, G3, _)))
    "1. Nf3"                                  in movesAllowed(G1->F3)(Set(Piece(White, Knight, F3, _)))
    "1. g3 a6 2. Bh3"                         in movesAllowed(G2->G3, A7->A6, F1->H3)(Set(Piece(White, Pawn, G3, _), Piece(White, Bishop, H3, _), Piece(Black, Pawn, A6, _)))
    "1. d4 e5 2. dxe5 d6 3. Bg5 dxe5 4. Bxd8" in movesAllowed(D2->D4, E7->E5, D4->E5, D7->D6, C1->G5, D6->E5, G5->D8)(Set(Piece(White, Bishop, D8, _), Piece(Black, Pawn, E5, _)), nCaptured = 3)
  }

  "Pawn moves" should {
    "white pawn to g6 after pawn to g4"            in lastMoveNotAllowed(G2->G4, A7->A6, G4->G6)
    "white pawn capture non-diagonal"              in lastMoveNotAllowed(D2->D4, D7->D5, D4->D5)
    "pawn promotion"                               in movesAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, H7->H8->Queen)(Set(Piece(White, Queen, H8, _), Piece(Black, Rook, G6, _), Piece(Black, Pawn, E2, _)), nCaptured = 3)
    "pawn promotion on capture"                    in movesAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, H7->G8->Queen)(Set(Piece(White, Queen, G8, _), Piece(Black, Rook, G6, _), Piece(Black, Pawn, E2, _)), nCaptured = 4)
    "white pawn to g8 without promotion"           in lastMoveNotAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, H7->H8)
    "en passant when pawn advanced two"            in movesAllowed(E2->E4, E7->E6, E4->E5, D7->D5, E5->D6)(Set(Piece(White, Pawn, D6, _), Piece(Black, Pawn, E6, _)), nCaptured = 1)
    "en passant when pawn previously advanced two" in lastMoveNotAllowed(E2->E4, D7->D5, E4->E5, E7->E6, E5->D6)
    "en passant when pawn advanced one"            in lastMoveNotAllowed(E2->E4, D7->D6, E4->E5, D6->D5, E5->D6)
  }

  "Castling" should {

    "1. Nf3 Nf6 2. g3 g6 3. Bh3 Bh6 4. O-O O-O; White and black castle kingside." in
      movesAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, `O-O`, `O-O`)(Set(Piece(White, Knight, F3, _),  Piece(White, Pawn, G3, _),  Piece(White, Bishop, H3, _),  Piece(White, King, G1, _),  Piece(White, Rook, F1, _),  Piece(Black, Knight, F6, _),  Piece(Black, Pawn, G6, _),  Piece(Black, Bishop, H6, _),  Piece(Black, King, G8, _),  Piece(Black, Rook, F8, _)))

    "1. Nc3 Nc6 2. d3 d6 3. Bg5 Bg4 4. Qd2 Qd7 5. O-O-O O-O-O; White and black castle queenside." in
      movesAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, `O-O-O`, `O-O-O`)(Set(Piece(White, Knight, C3, _),  Piece(White, Pawn, D3, _),  Piece(White, Bishop, G5, _),  Piece(White, Queen, D2, _),  Piece(White, Rook, D1, _),  Piece(White, King, C1, _),  Piece(Black, Knight, C6, _),  Piece(Black, Pawn, D6, _),  Piece(Black, Bishop, G4, _),  Piece(Black, Queen, D7, _),  Piece(Black, Rook, D8, _),  Piece(Black, King, C8, _)))

    "1. Nf3 Nf6 2. g3 g6 3. Bh3 Bh6 4. Kf1 Kf8 5. Ke1 Ke8 6. O-O; White attempts to castle kingside after moving the king." in
      lastMoveNotAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, E1->F1, E8->F8, F1->E1, F8->E8, `O-O`)

    "1. Nf3 Nf6 2. g3 g6 3. Bh3 Bh6 4. Rg1 Rg8 5. Rh1 Rh8 6. O-O; White attempts to castle kingside after moving the chosen rook." in
      lastMoveNotAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, H1->G1, H8->G8, G1->H1, G8->H8, `O-O`)

    "1. O-O; White attempts to castle kingside with pieces between the king and the chosen rook." in
      lastMoveNotAllowed(`O-O`)

    "1. f4 e5 2. g4 f5 3. Nf3 d5 4. Bh3 Qh4+ 5. O-O; White attempts to castle kingside with the king in check." in
      lastMoveNotAllowed(F2->F4, E7->E5, G2->G4, F7->F5, G1->F3, D7->D5, F1->H3, D8->H4, `O-O`)

    "1. f4 e5 2. fxe5 f6 3. exf6 Qxf6 4. g3 Qf5 5. Bg2 Qf6 6. Nh3 Qf5 7. O-O; White attempts to castle kingside passing through a square that is attacked by an enemy piece." in
      lastMoveNotAllowed(F2->F4, E7->E5, F4->E5, F7->F6, E5->F6, D8->F6, G2->G3, F6->F5, F1->G2, F5->F6, G1->H3, F6->F5, `O-O`)

  }

  "Check" should {

    "1. e4 Nf6 2. d4 Ng4 3. c4 Nxh2 4. f3 Nxf3+ 5. Nxf3; Capture the checking piece." in
      movesAllowed(E2 -> E4, G8 -> F6, D2 -> D4, F6 -> G4, C2 -> C4, G4 -> H2, F2 -> F3, H2 -> F3, G1 -> F3)(Set(Piece(White, Pawn, C4, _), Piece(White, Pawn, D4, _), Piece(White, Pawn, E4, _), Piece(White, Knight, F3, _)), nCaptured = 3)

    "1. e4 e5 2. d4 Bb4+ 3. Ke2; Move the White king out of check." in
      movesAllowed(E2 -> E4, E7 -> E5, D2 -> D4, F8 -> B4, E1 -> E2)(Set(Piece(White, King, E2, _), Piece(White, Pawn, D4, _), Piece(White, Pawn, E4, _), Piece(Black, Bishop, B4, _), Piece(Black, Pawn, E5, _)))

    "1. e4 e5 2. d4 Bb4+ 3. c3; Block the check." in
      movesAllowed(E2 -> E4, E7 -> E5, D2 -> D4, F8 -> B4, C2 -> C3)(Set(Piece(White, Pawn, C3, _), Piece(White, Pawn, D4, _), Piece(White, Pawn, E4, _), Piece(Black, Bishop, B4, _), Piece(Black, Pawn, E5, _)))

    "1. e4 e5 2. f4 Bb4 3. d3; The last move puts White's own king in check." in
      lastMoveNotAllowed(E2 -> E4, E7 -> E5, F2 -> F4, F8 -> B4, D2 -> D3)

    "1. e4 e5 2. d4 Bb4+ 3. e3; The last move leaves the White king in check." in
      lastMoveNotAllowed(E2 -> E4, E7 -> E5, D2 -> D4, F8 -> B4, E2 -> E3)

  }

  "Checkmate" should {

    "1. f3 e5 2. g4 Qh4#; Fool's mate." in
      { Board.initial.move(F2->F3, E7->E5, G2->G4, D8->H4) must beSome.which(_.isCheckmate) }

    "1. e4 e5 2. Qh5 Nc6 3. Bc4 Nf6? 4. Qxf7#; Scholars's mate." in
      { Board.initial.move(E2->E4, E7->E5, D1->H5, B8->C6, F1->C4, G8->F6, H5->F7) must beSome.which(_.isCheckmate) }

  }

  "Draws" should {

    "1. e3 a5 2. Qh5 Ra6 3. Qxa5 h5 4. h4 Rah6 5. Qxc7 f6 6. Qxd7+ Kf7 7. Qxb7 Qd3 8. Qxb8 Qh7 9. Qxc8 Kg6 10. Qe6; Stalemate." in
      { Board.initial.move(E2->E3, A7->A5, D1->H5, A8->A6, H5->A5, H7->H5, H2->H4, A6->H6, A5->C7, F7->F6, C7->D7, E8->F7, D7->B7, D8->D3, B7->B8, D3->H7, B8->C8, F7->G6, C8->E6) must beSome.which(b => b.isStalemate && b.isAutomaticDraw) }

    "1. Nf3 Nf6 2. Ng1 Ng8 3. Nf3 Nf6 4. Ng1 Ng8 5. Nf3 Nf6; Threefold repetition." in
      { Board.initial.move(G1->F3, G8->F6, F3->G1, F6->G8, G1->F3, G8->F6, F3->G1, F6->G8, G1->F3, G8->F6) must beSome.which(b => b.isThreefoldRepetition && b.mayClaimDraw) }
  }

  // Checks each move is generated and allowed
  def movesAllowed(moves: MoveBase*)(movedPieces: Set[Boolean => Piece] = Set(), nCaptured: Int = 0) = {
    def recur(board: Option[Board], remaining: List[MoveBase]): Result = {
      (board, remaining) match {
        case (Some(b), head :: tail) =>
          b.moves must contain(head)
          recur(b.move(head), tail)
        case (Some(b), Nil) =>
          onlyTheseMoved(movedPieces, nCaptured)(b)
        case (None, _) =>
          failure
      }
    }
    recur(Some(Board.initial), moves.toList)
  }

  // Checks each move except the last is generated and allowed
  def lastMoveNotAllowed(moves: MoveBase*) = {
    def recur(board: Option[Board], remaining: List[MoveBase]): Result = {
      (board, remaining) match {
        case (Some(b), last :: Nil) =>
          b.moves must not contain(last)
          recur(b.move(last), Nil)
        case (Some(b), head :: tail) =>
          b.moves must contain(head)
          recur(b.move(head), tail)
        case (Some(b), Nil) =>
          failure
        case (None, head :: tail) =>
          failure
        case (None, Nil) =>
          success
      }
    }
    recur(Some(Board.initial), moves.toList)
  }

  // Only checks the number of captures as the actual pieces captured are determined by the initial board pieces and the moved pieces specified
  def onlyTheseMoved(movedPieces: Set[Boolean => Piece] = Set(), nCaptured: Int = 0): Board => Result = {
    val initialPieces = Board.initial.pieces
    (board) => {
      val stationaryPieces = board.pieces.filterNot(_.hasMoved)
      stationaryPieces.size + movedPieces.size + nCaptured must beEqualTo(32)
      initialPieces must containAllOf(stationaryPieces.toSeq)
      board.pieces must containAllOf(movedPieces.map(_(true)).toSeq)
    }
  }

  def moveAndCheckMoves(moves: MoveBase*)(expectedMoves: MoveBase*) =
    Board.initial.move(moves.toList) must beSome.which(_.moves must containTheSameElementsAs(expectedMoves))

}
