package mateinone

import org.specs2.mutable._
import Square._
import File._
import Rank._
import MoveImplicits._
import org.specs2.execute.Result

class MateInOneSpec extends Specification {

  "Square" should {
    "square" in { E3 must beEqualTo(square(E,_3)) }
    "+" in {
      A1 + (2, 2) must beEqualTo(C3)
      A1 + (7, 7) must beEqualTo(H8)
      C3 + (-2, -2) must beEqualTo(A1)
      H8 + (-7, -7) must beEqualTo(A1)
      H8 + (0, -1) must beEqualTo(H7)
    }
  }

  "Board" should {

    "have the correct initial moves for white" in
      moveAndCheckMoves()(A2->A3, A2->A4, B1->A3, B1->C3, B2->B3, B2->B4, C2->C3, C2->C4, D2->D3, D2->D4, E2->E3, E2->E4, F2->F3, F2->F4, G1->F3, G1->H3, G2->G3, G2->G4, H2->H3, H2->H4)

    "have the correct initial moves for black" in
      moveAndCheckMoves(G2->G3)(A7->A6, A7->A5, B8->A6, B8->C6, B7->B6, B7->B5, C7->C6, C7->C5, D7->D6, D7->D5, E7->E6, E7->E5, F7->F6, F7->F5, G8->F6, G8->H6, G7->G6, G7->G5, H7->H6, H7->H5)

    "be immutable" in
      { Board.initial.move(G2->G3) must beSome.which { b => b.move(G3->G4); onlyTheseMoved(Set((White, Pawn, G3)))(b) } }

  }

  "Simple moves" should {
    "1. g3"                                   in movesAllowed(G2->G3)(Set((White, Pawn, G3)))
    "1. Nf3"                                  in movesAllowed(G1->F3)(Set((White, Knight, F3)))
    "1. g3 a6 2. Bh3"                         in movesAllowed(G2->G3, A7->A6, F1->H3)(Set((White, Pawn, G3), (White, Bishop, H3), (Black, Pawn, A6)))
    "1. d4 e5 2. dxe5 d6 3. Bg5 dxe5 4. Bxd8" in movesAllowed(D2->D4, E7->E5, D4->E5, D7->D6, C1->G5, D6->E5, G5->D8)(Set((White, Bishop, D8), (Black, Pawn, E5)), nCaptured = 3)
  }

  "Pawn moves" should {
    "white pawn to g6 after pawn to g4"            in lastMoveNotAllowed(G2->G4, A7->A6, G4->G6)
    "white pawn capture non-diagonal"              in lastMoveNotAllowed(D2->D4, D7->D5, D4->D5)
    "pawn promotion"                               in movesAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, H7->H8->Queen)(Set((White, Queen, H8), (Black, Rook, G6), (Black, Pawn, E2)), nCaptured = 3)
    "pawn promotion on capture"                    in movesAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, H7->G8->Queen)(Set((White, Queen, G8), (Black, Rook, G6), (Black, Pawn, E2)), nCaptured = 4)
    "white pawn to g8 without promotion"           in lastMoveNotAllowed(G2->G4, H7->H5, H2->H4, H5->G4, H4->H5, H8->H6, F2->F3, H6->G6, H5->H6, G4->F3, H6->H7, F3->E2, H7->H8)
  }

  "En passant" should {

    "1. e4 e6 2. e5 d5 3. exd6; White captures the black pawn en passant." in
      movesAllowed(E2->E4, E7->E6, E4->E5, D7->D5, E5->D6)(Set((White, Pawn, D6), (Black, Pawn, E6)), nCaptured = 1)

    "1. e4 d5 2. e5 g5 3. exd6; White attempts to capture the black pawn en passant, but in the wrong file." in
      lastMoveNotAllowed(E2->E4, D7->D5, E4->E5, G7->G5, E5->D6)

    "1. e4 d5 2. e5 e6 3. exd6; White attempts to capture the black pawn en passant, but it did not just move." in
      lastMoveNotAllowed(E2->E4, D7->D5, E4->E5, E7->E6, E5->D6)

    "1. e4 d6 2. e5 d5 3. exd6; White attempts to capture the black pawn en passant, but it only advanced one square." in
      lastMoveNotAllowed(E2->E4, D7->D6, E4->E5, D6->D5, E5->D6)

  }

  "Castling" should {

    "1. Nf3 Nf6 2. g3 g6 3. Bh3 Bh6 4. O-O O-O; White and black castle kingside." in
      movesAllowed(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, `O-O`, `O-O`)(Set((White, Knight, F3),  (White, Pawn, G3),  (White, Bishop, H3),  (White, King, G1),  (White, Rook, F1),  (Black, Knight, F6),  (Black, Pawn, G6),  (Black, Bishop, H6),  (Black, King, G8),  (Black, Rook, F8)))

    "1. Nc3 Nc6 2. d3 d6 3. Bg5 Bg4 4. Qd2 Qd7 5. O-O-O O-O-O; White and black castle queenside." in
      movesAllowed(B1->C3, B8->C6, D2->D3, D7->D6, C1->G5, C8->G4, D1->D2, D8->D7, `O-O-O`, `O-O-O`)(Set((White, Knight, C3),  (White, Pawn, D3),  (White, Bishop, G5),  (White, Queen, D2),  (White, Rook, D1),  (White, King, C1),  (Black, Knight, C6),  (Black, Pawn, D6),  (Black, Bishop, G4),  (Black, Queen, D7),  (Black, Rook, D8),  (Black, King, C8)))

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
      movesAllowed(E2 -> E4, G8 -> F6, D2 -> D4, F6 -> G4, C2 -> C4, G4 -> H2, F2 -> F3, H2 -> F3, G1 -> F3)(Set((White, Pawn, C4), (White, Pawn, D4), (White, Pawn, E4), (White, Knight, F3)), nCaptured = 3)

    "1. e4 e5 2. d4 Bb4+ 3. Ke2; Move the White king out of check." in
      movesAllowed(E2 -> E4, E7 -> E5, D2 -> D4, F8 -> B4, E1 -> E2)(Set((White, King, E2), (White, Pawn, D4), (White, Pawn, E4), (Black, Bishop, B4), (Black, Pawn, E5)))

    "1. e4 e5 2. d4 Bb4+ 3. c3; Block the check." in
      movesAllowed(E2 -> E4, E7 -> E5, D2 -> D4, F8 -> B4, C2 -> C3)(Set((White, Pawn, C3), (White, Pawn, D4), (White, Pawn, E4), (Black, Bishop, B4), (Black, Pawn, E5)))

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

  "Draw" should {

    "1. e3 a5 2. Qh5 Ra6 3. Qxa5 h5 4. h4 Rah6 5. Qxc7 f6 6. Qxd7+ Kf7 7. Qxb7 Qd3 8. Qxb8 Qh7 9. Qxc8 Kg6 10. Qe6; Stalemate." in
      { Board.initial.move(E2->E3, A7->A5, D1->H5, A8->A6, H5->A5, H7->H5, H2->H4, A6->H6, A5->C7, F7->F6, C7->D7, E8->F7, D7->B7, D8->D3, B7->B8, D3->H7, B8->C8, F7->G6, C8->E6) must beSome.which(b => b.isStalemate && b.isAutomaticDraw) }

    "1. Nf3 Nf6 2. Ng1 Ng8 3. Nf3 Nf6 4. Ng1 Ng8; Threefold repetition (opening is the first repetition)." in
      { Board.initial.move(G1->F3, G8->F6, F3->G1, F6->G8, G1->F3, G8->F6, F3->G1, F6->G8) must beSome.which(b => b.isThreefoldRepetition && b.mayClaimDraw) }

    "1. Nf3 Nf6 2. Ng1 Ng8 3. Nf3 Nf6 4. Ng1 Ng8 5. a4; Did not claim threefold repetition." in
      { Board.initial.move(G1->F3, G8->F6, F3->G1, F6->G8, G1->F3, G8->F6, F3->G1, F6->G8, A2->A4) must beSome.which(b => !b.isThreefoldRepetition) }

    "1. Nf3 Nf6 2. Ng1 Ng8 ... 49. Nf3 Nf6 50. Ng1 Ng8; Fifty-move rule." in
      { val moves: List[MoveBase] = List(G1->F3, G8->F6, F3->G1, F6->G8)
        Board.initial.move(List.fill(25)(moves).flatten) must beSome.which(b => b.isFiftyMoveRule && b.mayClaimDraw) }

  }

  "TerminalPrinter" should {
    "print the board" in {
      import TerminalPrinter._
      val expected =
        """|┌─────────────────┐
           |│ ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ │
           |│ ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ │
           |│                 │
           |│                 │
           |│ ♙               │
           |│                 │
           |│ · ♙ ♙ ♙ ♙ ♙ ♙ ♙ │
           |│ ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ │
           |└─────────────────┘""".stripMargin
      val move = Move.move(A2, A4)
      Board.initial.move(move).get.print(move) must beEqualTo(expected)
    }
  }

  "Evaluation" should {
    "evaluate the initial board" in { Evaluation.value(Board.initial) must beEqualTo(0) }
    "evaluate after a pawn move" in {
      val m = Move.move(A2, A4)
      val b = Board.initial.move(m).get
      Evaluation.value(b) must beEqualTo(-5)
      Evaluation.deltaValue(Board.initial, m) must beEqualTo(-5)
    }
    "evaluate after a knight move" in {
      val m = Move.move(C6, D4)
      val b = Board.initial.move(Move.move(B1, C3), Move.move(B8, C6), Move.move(G1, F3), Move.move(G8, F6), Move.move(D2, D4)).get
      Evaluation.value(b.move(m).get) must beEqualTo(-90)
      Evaluation.deltaValue(b, m) must beEqualTo(-130)
    }
  }

  // Checks each move is generated and allowed
  def movesAllowed(moves: MoveBase*)(movedPieces: Set[(Side, PieceType, Square)] = Set(), nCaptured: Int = 0) = {
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
          b.moves.toSet must not contain(last)
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

  def onlyTheseMoved(movedPieces: Set[(Side, PieceType, Square)] = Set(), nCaptured: Int = 0): Board => Result = {
    val initialPieces = Board.initial.pieces
    (board) => {
      val stationaryPieces = board.pieces & initialPieces
      stationaryPieces.size + movedPieces.size + nCaptured must beEqualTo(32)
      stationaryPieces &~ initialPieces must beEmpty
      movedPieces &~ board.pieces must beEmpty
    }
  }

  def moveAndCheckMoves(moves: MoveBase*)(expectedMoves: MoveBase*) =
    Board.initial.move(moves.toList) must beSome.which(_.moves must containTheSameElementsAs(expectedMoves))

}
