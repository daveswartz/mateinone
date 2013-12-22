package mateinone

import org.specs2.mutable._
import mateinone.Board.OccupiedPath
import org.specs2.specification.Scope
import Square._

class OccupiedPathSpec extends Specification {
  "OccupiedPath" should {
    "vacate squares" in new OccupiedPathScope {
      occupiedPath.vacate(g2) must beEqualTo(OccupiedPath(path, Set()))
    }
    "occupy squares" in new OccupiedPathScope {
      occupiedPath.occupy(h3) must beEqualTo(OccupiedPath(path, Set(g2, h3)))
    }
    "determine valid move squares" in new OccupiedPathScope {
      occupiedPath.vacate(g2).validEnds must beEqualTo(Set(g2, h3))
    }
  }
}

trait OccupiedPathScope extends Scope {
  val path = List(g2, h3)
  val occupied = Set(g2)
  val occupiedPath = OccupiedPath(path, occupied)
}

class BoardSpec extends Specification { // TODO add some simple move generation testing (e.g., test all valid moves that can be gen right before a castle).
  import Piece._

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
    "have the correct state after construction" in new BoardScope {
      Board().pieces must haveSize(16)
      Board().moves must haveSize(20)
    }
    "allow knight to f3" in new BoardScope {
      Board().moveSquares(g1->f3) must beSome.which(onlyTheseMoved(knight->f3))
    }
    "allow pawn to g3" in new BoardScope {
      Board().moveSquares(g2->g3) must beSome.which(onlyTheseMoved(pawn->g3))
    }
    "allow bishop to h3" in new BoardScope {
      Board().moveSquares(g2->g3, f1->h3) must beSome.which(onlyTheseMoved(pawn->g3, bishop->h3))
    }
    "allow kingside castle" in new BoardScope {
      Board().move(N_to_f3, P_to_g3, B_to_h3, KingsideCastle) must beSome.which(onlyTheseMoved(knight->f3, pawn->g3, bishop->h3, king->g1, rook->f1))
    }
    "allow queenside castle" in new BoardScope {
      Board().move(N_to_a3, P_to_d3, B_to_g5, Q_to_d2, QueensideCastle) must beSome.which(onlyTheseMoved(knight->a3, pawn->d3, bishop->g5, queen->d2, rook->d1, king->c1))
    }
    "allow pawn to g4 then not allow pawn to g6" in new BoardScope {
      val after_g4 = Board().moveSquares(g2->g4)
      after_g4 must beSome
      after_g4.get.moveSquares(g4->g6) must beNone
    }
  } // TODO test trying to castle after moving, and other invalid moves

}

trait BoardScope extends Scope {
  val N_to_a3 = SimpleMove(Piece(Knight, b1, hasMoved = false), a3)
  val N_to_f3 = SimpleMove(Piece(Knight, g1, hasMoved = false), f3)
  val Q_to_d2 = SimpleMove(Piece(Queen, d1, hasMoved = false), d2)
  val P_to_d3 = SimpleMove(Piece(Pawn, d2, hasMoved = false), d3)
  val P_to_g3 = SimpleMove(Piece(Pawn, g2, hasMoved = false), g3)
  val B_to_g5 = SimpleMove(Piece(Bishop, c1, hasMoved = false), g5)
  val B_to_h3 = SimpleMove(Piece(Bishop, f1, hasMoved = false), h3)
}
