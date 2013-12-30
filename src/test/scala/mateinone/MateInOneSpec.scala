package mateinone

import org.specs2.mutable._
import org.specs2.specification.Scope
import Square._
import SimpleMove._
import Castle._

class OccupiedPathSpec extends Specification {
  "OccupiedPath" should {
    "vacate squares" in new OccupiedPathScope {
      occupiedPath.vacate(Set(g2)) must beEqualTo(OccupiedPath(path, Set()))
    }
    "occupy squares" in new OccupiedPathScope {
      occupiedPath.occupy(Set(h3)) must beEqualTo(OccupiedPath(path, Set(g2, h3)))
    }
    "determine valid move squares" in new OccupiedPathScope {
      occupiedPath.vacate(Set(g2)).validEnds must beEqualTo(Set(g2, h3))
    }
  }
}

trait OccupiedPathScope extends Scope {
  val path = List(g2, h3)
  val occupied = Set(g2)
  val occupiedPath = OccupiedPath(path, occupied)
}

class BoardSpec extends Specification {

  def piece(side: Side, hasMoved: Boolean)(pieceTypeAndSquare: (PieceType, Square)): Piece =
    pieceTypeAndSquare match { case (pieceType, square) => Piece(side, pieceType, square, hasMoved) }

  def onlyTheseMoved(white: Set[(PieceType, Square)] = Set(), black: Set[(PieceType, Square)] = Set()): Board => Boolean = {

    val whiteMoved = piece(White, hasMoved = true) _
    val blackMoved = piece(Black, hasMoved = true) _

    val initialPieces = Board().pieces
    val movedPieces = white.map(whiteMoved) ++ black.map(blackMoved)

    (board) => {
      val stationaryPieces = board.pieces.filterNot(_.hasMoved)
      stationaryPieces.size + movedPieces.size == 32 &&     // the total number of pieces is 32
        stationaryPieces.forall(initialPieces.contains) &&  // all unmoved pieces are at the initial state
        movedPieces.forall(board.pieces.contains)           // all moved pieces are in the specified state
    }

  }

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
    "have the correct initial moves for white" in {
      val expectedMoves: Seq[SimpleMove] = Seq(
        a2->a3, a2->a4, b1->a3, b1->c3, b2->b3, b2->b4, c2->c3, c2->c4, d2->d3, d2->d4,
        e2->e3, e2->e4, f2->f3, f2->f4, g1->f3, g1->h3, g2->g3, g2->g4, h2->h3, h2->h4
      )
      Board().moves must containTheSameElementsAs(expectedMoves)
    }
    "have the correct initial moves for black" in {
      val expectedMoves: Seq[SimpleMove] = Seq(
        a7->a6, a7->a5, b8->a6, b8->c6, b7->b6, b7->b5, c7->c6, c7->c5, d7->d6, d7->d5,
        e7->e6, e7->e5, f7->f6, f7->f5, g8->f6, g8->h6, g7->g6, g7->g5, h7->h6, h7->h5
      )
      Board().move(g2->g3) must beSome.which(_.moves must containTheSameElementsAs(expectedMoves))
    }
    "allow pawn to g3" in {
      Board().move(g2->g3) must beSome.which(onlyTheseMoved(white = Set(Pawn->g3)))
    }
    "allow knight to f3" in {
      Board().move(g1->f3) must beSome.which(onlyTheseMoved(white = Set(Knight->f3)))
    }
    "allow bishop to h3" in {
      Board().move(g2->g3, a7->a6, f1->h3) must beSome.which(onlyTheseMoved(white = Set(Pawn->g3, Bishop->h3), black = Set(Pawn->a6)))
    }
    "allow castle kingside for white" in {
      Board().move(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, `O-O`) must beSome.which(onlyTheseMoved(white = Set(Knight->f3, Pawn->g3, Bishop->h3, King->g1, Rook->f1), black = Set(Pawn->a4)))
    }
    "allow castle queenside for white" in {
      Board().move(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, `O-O-O`) must beSome.which(onlyTheseMoved(white = Set(Knight->c3, Pawn->d3, Bishop->g5, Queen->d2, Rook->d1, King->c1), black = Set(Pawn->a3)))
    }
    "allow promotion of a pawn on the 8th rank" in {
      Board().move(g2->g4, g4->g5, g5->g6, g6->g7, g7->g8 promote Queen) must beSome.which(onlyTheseMoved(white = Set(Queen->g8)))
    }.pendingUntilFixed("Failing as the moves are blocked by black pieces (cannot fix w/o promotion)")
    "require promotion of a pawn on the 8th rank" in {
      Board().move(g2->g4, g4->g5, g5->g6, g6->g7) must beSome.which(_.move(g7->g8) must beNone)
    }.pendingUntilFixed("Failing as the moves are blocked by black pieces, not because or the requirement (cannot fix w/o promotion)")
    "not allow pawn to g6 after pawn to g4" in {
      Board().move(g2->g4, g4->g6) must beNone
    }
    "not allow castle kingside for white after moving the king" in {
      Board().move(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, e1->f1, a4->a3) must beSome.which(_.move(`O-O`) must beNone)
    }
    "not allow castle kingside for white after moving the rook" in {
      Board().move(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4, h1->g1, a4->a3, g1->h1, b7->b6) must beSome.which(_.move(`O-O`) must beNone)
    }
    "not allow castle queenside for white after moving the king" in {
      Board().move(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, e1->d1, b7->b6) must beSome.which(_.move(`O-O-O`) must beNone)
    }
    "not allow castle queenside for white after moving the rook" in {
      Board().move(b1->c3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, a4->a3, a1->b1, b7->b6, b1->a1, b6->b5) must beSome.which(_.move(`O-O-O`) must beNone)
    }
    "generate castling kingside" in {
      Board().move(g1->f3, a7->a6, g2->g3, a6->a5, f1->h3, a5->a4) must beSome.which { board =>
        val simple: SimpleMove = e1->g1
        board.moves must contain(`O-O`) and not contain simple
      }
    }
    "generate castling queenside" in {
      val simple: SimpleMove = e1->c1
      Board().move(b1->a3, a7->a6, d2->d3, a6->a5, c1->g5, a5->a4, d1->d2, b7->b6) must beSome.which(_.moves must contain(`O-O-O`) and not contain simple)
    }
    "be immutable" in {
      val after_g3 = Board().move(g2->g3)
      after_g3.get.move(g3->g4)
      after_g3 must beSome.which(onlyTheseMoved(white = Set(Pawn->g3)))
    }
    // TODO add tests to cover what is causing the chess.scala script to fail
    // TODO Test castle for black side
  }

}
