package mateinone

import org.specs2.mutable._
import Square._
import SimpleMove._
import Castle._
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

  // TODO pull out common moves
  // TODO nest to reduce repeated wording e.g., White { Allow Move { Pawn to g3, knight to f3 }, Not Allow Move {} }, Black {}
  // TODO test castling for black side

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

    "be immutable" in {
      Board().move(g2->g3) must beSome.which { b =>
        b.move(g3->g4)
        onlyTheseMoved(white = Set(Pawn->g3))(b)
      }
    }

    "allow pawn to g3" in moveAndCheckMoved(g2->g3)(white = Set(Pawn->g3))
    "allow knight to f3" in moveAndCheckMoved(g1->f3)(white = Set(Knight->f3))
    "allow bishop to h3" in moveAndCheckMoved(g2->g3, a7->a6, f1->h3)(white = Set(Pawn->g3, Bishop->h3), black = Set(Pawn->a6))
    "allow castling kingside for white" in moveAndCheckMoved(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, `O-O`)(white = Set(Knight->f3, Pawn->g3, Bishop->h3, King->g1, Rook->f1), black = Set(Pawn->a4))
    "allow castling queenside for white" in moveAndCheckMoved(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, `O-O-O`)(white = Set(Knight->c3, Pawn->d3, Bishop->g5, Queen->d2, Rook->d1, King->c1), black = Set(Pawn->a3))
    "allow promotion of a pawn on the 8th rank" in moveAndCheckMoved(g2->g4, g4->g5, g5->g6, g6->g7, g7->g8 promote Queen)(white = Set(Queen->g8)).pendingUntilFixed("Failing as the moves are blocked by black pieces (cannot fix w/o promotion)")

    "not allow a two file kingside move for white that is not castling" in moveAndEnsureInvalidLastMove(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, e1->g1)
    "not allow a two file queenside move for white that is not castling" in moveAndEnsureInvalidLastMove(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, e1->c1)
    "not allow pawn to g6 after pawn to g4" in moveAndEnsureInvalidLastMove(g2->g4, a7->a6, g4->g6)
    "require promotion of a pawn on the 8th rank" in moveAndEnsureInvalidLastMove(g2->g4, g4->g5, g5->g6, g6->g7, g7->g8).pendingUntilFixed("Failing as the moves are blocked by black pieces, not because or the requirement (cannot fix w/o promotion)")
    "not allow castling kingside for white after moving the king" in moveAndEnsureInvalidLastMove(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, e1->f1, a4->a3, f1->e1, b7->b6, `O-O`)
    "not allow castling kingside for white after moving the rook" in moveAndEnsureInvalidLastMove(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, h1->g1, a4->a3, g1->h1, b7->b6, `O-O`)
    "not allow castling queenside for white after moving the king" in moveAndEnsureInvalidLastMove(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, e1->d1, b7->b6, `O-O-O`)
    "not allow castling queenside for white after moving the rook" in moveAndEnsureInvalidLastMove(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, a1->b1, b7->b6, b1->a1, b6->b5, `O-O-O`)

    "generate initial moves for white" in moveAndCheckMoves()(a2->a3, a2->a4, b1->a3, b1->c3, b2->b3, b2->b4, c2->c3, c2->c4, d2->d3, d2->d4, e2->e3, e2->e4, f2->f3, f2->f4, g1->f3, g1->h3, g2->g3, g2->g4, h2->h3, h2->h4)
    "generate initial moves for black" in moveAndCheckMoves(g2->g3)(a7->a6, a7->a5, b8->a6, b8->c6, b7->b6, b7->b5, c7->c6, c7->c5, d7->d6, d7->d5, e7->e6, e7->e5, f7->f6, f7->f5, g8->f6, g8->h6, g7->g6, g7->g5, h7->h6, h7->h5)
    "generate castling kingside for white" in moveAndCheckSomeMoves(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4)(`O-O`(White))
    "generate castling queenside for white" in moveAndCheckSomeMoves(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3)(`O-O-O`(White))
    // TODO generate promotion

    "not generate a two file kingside move for white that is not castling" in moveAndCheckNotSomeMoves(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4)(e1->g1)
    "not generate a two file queenside move for white that is not castling" in moveAndCheckNotSomeMoves(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3)(e1->c1)
    "not generate pawn to g6 after pawn to g4" in moveAndCheckNotSomeMoves(g2->g4, a7->a6)(g4->g6)
    // TODO not generate pawn move to the 8th rank that is not promotion
    "not generate castling kingside for white after moving the king" in moveAndCheckNotSomeMoves(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, e1->f1, a4->a3, f1->e1, b7->b6)(`O-O`(White))
    "not generate castling kingside for white after moving the rook" in moveAndCheckNotSomeMoves(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, h1->g1, a4->a3, g1->h1, b7->b6)(`O-O`(White))
    "not generate castling queenside for white after moving the king" in moveAndCheckNotSomeMoves(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, e1->d1, b7->b6)(`O-O-O`(White))
    "not generate castling queenside for white after moving the rook" in moveAndCheckNotSomeMoves(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, a1->b1, b7->b6, b1->a1, b6->b5)(`O-O-O`(White))

  }

  def piece(side: Side, hasMoved: Boolean)(pieceTypeAndSquare: (PieceType, Square)): Piece =
    pieceTypeAndSquare match { case (pieceType, square) => Piece(side, pieceType, square, hasMoved) }

  def onlyTheseMoved(white: Set[(PieceType, Square)] = Set(), black: Set[(PieceType, Square)] = Set()): Board => Result = {

    val whiteMoved = piece(White, hasMoved = true) _
    val blackMoved = piece(Black, hasMoved = true) _

    val initialPieces = Board().pieces
    val movedPieces = white.map(whiteMoved) ++ black.map(blackMoved)

    (board) => {
      val stationaryPieces = board.pieces.filterNot(_.hasMoved)
      stationaryPieces.size + movedPieces.size must beEqualTo(32)
      initialPieces must containAllOf(stationaryPieces.toSeq)
      board.pieces must containAllOf(movedPieces.toSeq)
    }

  }

  def moveAndCheckMoved(moves: Either[Move, Side => Move]*)(white: Set[(PieceType, Square)] = Set(), black: Set[(PieceType, Square)] = Set()) =
    Board().move(moves :_*) must beSome.which(onlyTheseMoved(white, black))

  def moveAndEnsureInvalidLastMove(moves: Either[Move, Side => Move]*) = Board().move(moves.init :_*) must beSome.which(_.move(moves.last) must beNone)

  def moveAndCheckMoves(moves: Either[Move, Side => Move]*)(expectedMoves: Move*) =
    Board().move(moves :_*) must beSome.which(_.moves must containTheSameElementsAs(expectedMoves))

  def moveAndCheckSomeMoves(moves: Either[Move, Side => Move]*)(expectedMoves: Move*) =
    Board().move(moves :_*) must beSome.which(_.moves must containAllOf(expectedMoves))

  def moveAndCheckNotSomeMoves(moves: Either[Move, Side => Move]*)(expectedMoves: Move*) =
    Board().move(moves :_*) must beSome.which(_.moves must containAllOf(expectedMoves) not)

}
