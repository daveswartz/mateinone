package mateinone

import org.specs2.mutable._
import Square._
import SimpleMove._
import Castle._
import Move._
import org.specs2.execute.Result

class OccupiedPathSpec extends Specification {

  val path = List(g2, h3)
  val occupied = Set(g2)
  val occupiedPath = OccupiedPath(path, occupied)

  "OccupiedPath" should {
    "vacate squares" in {
      occupiedPath.vacate(Set(g2)) must beEqualTo(OccupiedPath(path, Set()))
    }
    "occupy squares" in {
      occupiedPath.occupy(Set(h3)) must beEqualTo(OccupiedPath(path, Set(g2, h3)))
    }
    "determine valid move squares" in {
      occupiedPath.vacate(Set(g2)).validEnds must beEqualTo(Set(g2, h3))
    }
  }
}

class BoardSpec extends Specification {

  "Board" should {

    "have the correct initial pieces" in {
      val expectedPieces: Seq[Piece] =
        Seq(
          Rook->a1, Pawn->a2, Knight->b1, Pawn->b2, Bishop->c1, Pawn->c2, Queen->d1, Pawn->d2,
          King->e1, Pawn->e2, Bishop->f1, Pawn->f2, Knight->g1, Pawn->g2, Rook->h1, Pawn->h2
        ).map(piece(White, hasMoved = false)) ++
        Seq(
          Rook->a8, Pawn->a7, Knight->b8, Pawn->b7, Bishop->c8, Pawn->c7, Queen->d8, Pawn->d7,
          King->e8, Pawn->e7, Bishop->f8, Pawn->f7, Knight->g8, Pawn->g7, Rook->h8, Pawn->h7
        ).map(piece(Black, hasMoved = false))
      Board().pieces must containTheSameElementsAs(expectedPieces)
    }

    "have the correct initial moves for white" in moveAndCheckMoves()(a2->a3, a2->a4, b1->a3, b1->c3, b2->b3, b2->b4, c2->c3, c2->c4, d2->d3, d2->d4, e2->e3, e2->e4, f2->f3, f2->f4, g1->f3, g1->h3, g2->g3, g2->g4, h2->h3, h2->h4)
    "have the correct initial moves for black" in moveAndCheckMoves(g2->g3)(a7->a6, a7->a5, b8->a6, b8->c6, b7->b6, b7->b5, c7->c6, c7->c5, d7->d6, d7->d5, e7->e6, e7->e5, f7->f6, f7->f5, g8->f6, g8->h6, g7->g6, g7->g5, h7->h6, h7->h5)

    "be immutable" in {
      Board().move(g2->g3) must beSome.which { b =>
        b.move(g3->g4)
        onlyTheseMoved(white = Set(Pawn->g3))(b)
      }
    }

    "g3" in movesAllowed(g2->g3)(white = Set(Pawn->g3))
    "Nf3" in movesAllowed(g1->f3)(white = Set(Knight->f3))
    "Bh3" in movesAllowed(g2->g3, a7->a6, f1->h3)(white = Set(Pawn->g3, Bishop->h3), black = Set(Pawn->a6))
    "O-O for both sides" in movesAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, `O-O`)(white = Set(Knight->f3, Pawn->g3, Bishop->h3, King->g1, Rook->f1), black = Set(Knight->f6, Pawn->g6, Bishop->h6, King->g8, Rook->f8))
    "O-O-O for both sides" in movesAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, `O-O-O`, `O-O-O`)(white = Set(Knight->c3, Pawn->d3, Bishop->g5, Queen->d2, Rook->d1, King->c1), black = Set(Knight->c6, Pawn->d6, Bishop->g4, Queen->d7, Rook->d8, King->c8))
    "pawn promotion" in movesAllowed(g2->g4, g4->g5, g5->g6, g6->g7, g7->g8 promote Queen)(white = Set(Queen->g8)).pendingUntilFixed("Failing as the moves are blocked by black pieces (cannot fix w/o promotion)")

    "1. d4 e5 2. dxe5 d6 3. Bg5 dxe5 4. Bxe8" in movesAllowed(d2->d4, e7->e5, d4->e5, d7->d6, c1->g5, d6->e5, g5->d8)(white = Set(Bishop->e8), black = Set(Pawn->e5), nCaptured = 3).pendingUntilFixed("Requires capture")

    "white king to g1 without castling" in lastMoveNotAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, e1->g1)
    "black king to g8 without castling" in lastMoveNotAllowed(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, e8->g8)
    "white king to c1 without castling" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, e1->c1)
    "black king to c8 without castling" in lastMoveNotAllowed(b1->c3, b8->c6, d2->d3, d7->d6, c1->g5, c8->g4, d1->d2, d8->d7, `O-O-O`, e8->c8)
    "white pawn to g6 after pawn to g4" in lastMoveNotAllowed(g2->g4, a7->a6, g4->g6)
    "white pawn to g8 without promotion" in lastMoveNotAllowed(g2->g4, g4->g5, g5->g6, g6->g7, g7->g8).pendingUntilFixed("Failing as the moves are blocked by black pieces, not because or the requirement (cannot fix w/o promotion)")
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

  }

  // Checks each move is generated and allowed
  def movesAllowed(moves: Either[Move, Side => Move]*)(white: Set[(PieceType, Square)] = Set(), black: Set[(PieceType, Square)] = Set(), nCaptured: Int = 0) = {
    def recur(board: Option[Board], remaining: List[Either[Move, Side => Move]]): Result = {
      (board, remaining) match {
        case (Some(b), head :: tail) =>
          b.moves must contain(toMove(head, b.turn))
          recur(b.move(head), tail)
        case (Some(b), Nil) =>
          onlyTheseMoved(white, black, nCaptured)(b)
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

  def piece(side: Side, hasMoved: Boolean)(pieceTypeAndSquare: (PieceType, Square)): Piece =
    pieceTypeAndSquare match { case (pieceType, square) => Piece(side, pieceType, square, hasMoved) }

  // Only checks the number of captures as the actual pieces captured are determined by the initial board pieces and the moved pieces specified
  def onlyTheseMoved(white: Set[(PieceType, Square)] = Set(), black: Set[(PieceType, Square)] = Set(), nCaptured: Int = 0): Board => Result = {

    val whiteMoved = piece(White, hasMoved = true) _
    val blackMoved = piece(Black, hasMoved = true) _

    val initialPieces = Board().pieces
    val movedPieces = white.map(whiteMoved) ++ black.map(blackMoved)

    (board) => {
      val stationaryPieces = board.pieces.filterNot(_.hasMoved)
      stationaryPieces.size + movedPieces.size + nCaptured must beEqualTo(32)
      initialPieces must containAllOf(stationaryPieces.toSeq)
      board.pieces must containAllOf(movedPieces.toSeq)
    }

  }

  def moveAndCheckMoves(moves: Either[Move, Side => Move]*)(expectedMoves: Move*) =
    Board().move(moves :_*) must beSome.which(_.moves must containTheSameElementsAs(expectedMoves))

  def moveAndCheckNotSomeMoves(moves: Either[Move, Side => Move]*)(expectedMoves: Move*) =
    Board().move(moves :_*) must beSome.which(_.moves must containAllOf(expectedMoves) not)

}
