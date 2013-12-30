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

  def piece(side: Side, hasMoved: Boolean)(pieceTypeAndSquare: (PieceType, Square)) =
    pieceTypeAndSquare match { case (pieceType, square) => Piece(side, pieceType, square, hasMoved) }

  def onlyTheseMoved(pieceTypeAndSquares: (PieceType, Square)*): Board => Boolean = {
    val initial = Board().pieces
    val movedPieces = pieceTypeAndSquares.toSet.map(piece(White, hasMoved = true))
    (board) => {
      val unmovedPieces = board.pieces.filterNot(_.hasMoved)
      unmovedPieces.size + movedPieces.size == 32 &&  // the total number of pieces is 32
        unmovedPieces.forall(initial.contains) &&     // all unmoved pieces are at the initial state
        movedPieces.forall(board.pieces.contains)     // all moved pieces are in the specified state
    }
  }

  "Board" should {
    "have the correct state after construction" in {
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
      val expectedMoves: Seq[SimpleMove] = Seq(
        a2->a3, a2->a4, b1->a3, b1->c3, b2->b3, b2->b4, c2->c3, c2->c4, d2->d3, d2->d4,
        e2->e3, e2->e4, f2->f3, f2->f4, g1->f3, g1->h3, g2->g3, g2->g4, h2->h3, h2->h4  // TODO good but fails as now generating all moves, no turn concept, add test after single white move
      )
      Board().moves must containTheSameElementsAs(expectedMoves)
    }
    "allow pawn to g3" in {
      Board().move(g2->g3) must beSome.which(onlyTheseMoved(Pawn->g3))
    }
    "allow knight to f3" in {
      Board().move(g1->f3) must beSome.which(onlyTheseMoved(Knight->f3))
    }
    "allow bishop to h3" in {
      Board().move(g2->g3, f1->h3) must beSome.which(onlyTheseMoved(Pawn->g3, Bishop->h3))
    }
    "allow castle kingside" in {
      Board().move(g1->f3, g2->g3, f1->h3, `O-O`) must beSome.which(onlyTheseMoved(Knight->f3, Pawn->g3, Bishop->h3, King->g1, Rook->f1))
    }
    "allow castle queenside" in {
      Board().move(b1->a3, d2->d3, c1->g5, d1->d2, `O-O-O`) must beSome.which(onlyTheseMoved(Knight->a3, Pawn->d3, Bishop->g5, Queen->d2, Rook->d1, King->c1))
    }
    "allow promotion of a pawn on the 8th rank" in { // TODO failing as blocked by black pieces
      Board().move(g2->g4, g4->g5, g5->g6, g6->g7, g7->g8 promote Queen) must beSome.which(onlyTheseMoved(Queen->g8))
    }
    "require promotion of a pawn on the 8th rank" in { // TODO failing as blocked by black, not because of requirement
      Board().move(g2->g4, g4->g5, g5->g6, g6->g7, g7->g8) must beNone
    }
    "not allow pawn to g6 after pawn to g4" in {
      Board().move(g2->g4, g4->g6) must beNone
    }
    "not allow castle kingside after moving the king" in {
      Board().move(g1->f3, g2->g3, f1->h3, e1->f2, `O-O`) must beNone
    }
    "not allow castle queenside after moving the king" in {
      Board().move(b1->a3, d2->d3, c1->g5, d1->d2, e1->d1, `O-O-O`) must beNone
    }
    "generate castling kingside" in {
      val moves = Board().move(g1->f3, g2->g3, f1->h3).get.moves
      moves must contain(`O-O`)
      val simple: SimpleMove = e1->g1
      moves must not contain simple
    }
    "generate castling queenside" in {
      val moves = Board().move(b1->a3, d2->d3, c1->g5, d1->d2).get.moves
      moves must contain(`O-O-O`)
      val simple: SimpleMove = e1->c1
      moves must not contain simple
    }
    "be immutable" in {
      val after_g3 = Board().move(g2->g3)
      after_g3.get.move(g3->g4)
      after_g3 must beSome.which(onlyTheseMoved(Pawn->g3))
    }
    // TODO Test that after the rook moves a castle is not allowed and not generated
  }

}
