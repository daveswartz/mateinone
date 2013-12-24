package mateinone

import org.specs2.mutable._
import mateinone.Board.OccupiedPath
import org.specs2.specification.Scope
import Square._

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
  import Piece._
  import SimpleMove._
  import Castle._

  def onlyTheseMoved(ofTypeAndSquare: ((Square, Boolean) => Piece, Square)*): Board => Boolean = {
    def toPiece(ofTypeAndSquare: ((Square, Boolean) => Piece, Square)): Piece = ofTypeAndSquare match { case (ofType, s) => moved(ofType)(s) }
    val movedPieces = ofTypeAndSquare.toSet.map(toPiece)
    val initial = Board().pieces
    (board) => {
      val unmovedPieces = board.pieces.filterNot(_.hasMoved)
      unmovedPieces.size + movedPieces.size == 16 &&  // the total number of pieces is 16
        unmovedPieces.forall(initial.contains) &&     // all unmoved pieces are at the initial state
        movedPieces.forall(board.pieces.contains)     // all moved pieces are in the specified state
    }
  }

  "Board" should {
    "have the correct state after construction" in {
      Board().pieces must haveSize(16)
      Board().moves must haveSize(20)
    }
    "allow pawn to g3" in {
      Board().move(g2->g3) must beSome.which(onlyTheseMoved(pawn->g3))
    }
    "allow knight to f3" in {
      Board().move(g1->f3) must beSome.which(onlyTheseMoved(knight->f3))
    }
    "allow bishop to h3" in {
      Board().move(g2->g3, f1->h3) must beSome.which(onlyTheseMoved(pawn->g3, bishop->h3))
    }
    "allow castle kingside" in {
      Board().move(g1->f3, g2->g3, f1->h3, `O-O`) must beSome.which(onlyTheseMoved(knight->f3, pawn->g3, bishop->h3, king->g1, rook->f1))
    }
    "allow castle queenside" in {
      Board().move(b1->a3, d2->d3, c1->g5, d1->d2, `O-O-O`) must beSome.which(onlyTheseMoved(knight->a3, pawn->d3, bishop->g5, queen->d2, rook->d1, king->c1))
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
    "be immutable" in {
      val after_g3 = Board().move(g2->g3)
      after_g3.get.move(g3->g4)
      after_g3 must beSome.which(onlyTheseMoved(pawn->g3))
    }
    // TODO test a promotion
    // TODO add some simple move generation testing (e.g., test all valid moves that can be gen right before a castle).
  }

}
