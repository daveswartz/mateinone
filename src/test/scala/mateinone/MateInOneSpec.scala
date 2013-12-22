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

  def onlyTheseMoved(moved: Set[Piece]): Board => Boolean = {
    val initial = Board().pieces
    (board) => {
      val unmoved = board.pieces.filterNot(_.hasMoved)
      unmoved.size + moved.size == 16 &&     // the total number of pieces is 16
        unmoved.forall(initial.contains) &&  // all unmoved pieces are at the initial state
        moved.forall(board.pieces.contains)  // all moved pieces are in the specified state
    }
  }

  "Board" should {
    "have the correct state after construction" in new BoardScope {
      Board().pieces must haveSize(16)
      Board().moves must haveSize(20)
    }
    "allow knight to f3" in new BoardScope {
      Board().move(N_to_f3) must beSome.which(onlyTheseMoved(Set(N_at_f3)))
    }
    "allow pawn to g3" in new BoardScope {
      Board().move(P_to_g3) must beSome.which(onlyTheseMoved(Set(P_at_g3)))
    }
    "allow bishop to h3" in new BoardScope {
      Board().moves(P_to_g3 :: B_to_h3 :: Nil) must beSome.which(onlyTheseMoved(Set(P_at_g3, B_at_h3)))
    }
    "allow kingside castle" in new BoardScope {
      Board().moves(N_to_f3 :: P_to_g3 :: B_to_h3 :: KingsideCastle :: Nil) must beSome.which(onlyTheseMoved(Set(N_at_f3, P_at_g3, B_at_h3, K_at_g1, R_at_f1)))
    }
    "allow queenside castle" in new BoardScope {
      Board().moves(N_to_a3 :: P_to_d3 :: B_to_g5 :: Q_to_d2 :: QueensideCastle :: Nil) must beSome.which(onlyTheseMoved(Set(N_at_a3, P_at_d3, B_at_g5, Q_at_d2, R_at_d1, K_at_c1)))
    }
    "allow pawn to g4 then not allow pawn to g6" in new BoardScope {
      val after_g4 = Board().move(P_to_g4)
      after_g4 must beSome
      after_g4.get.move(P_to_g6) must beNone
    }
  } // TODO test trying to castle after moving, and other invalid moves

}

trait BoardScope extends Scope {

  val N_to_a3 = SimpleMove(Piece(Knight, b1), a3) // TODO b1 -> a3
  val N_to_f3 = SimpleMove(Piece(Knight, g1), f3)
  val Q_to_d2 = SimpleMove(Piece(Queen, d1), d2)
  val P_to_d3 = SimpleMove(Piece(Pawn, d2), d3)
  val P_to_g3 = SimpleMove(Piece(Pawn, g2), g3)
  val P_to_g4 = SimpleMove(Piece(Pawn, g2), g4)
  val P_to_g6 = SimpleMove(Piece(Pawn, g4), g6)
  val B_to_g5 = SimpleMove(Piece(Bishop, c1), g5)
  val B_to_h3 = SimpleMove(Piece(Bishop, f1), h3)

  val N_at_a3 = Piece(Knight, a3, true)
  val N_at_f3 = Piece(Knight, f3, true)
  val Q_at_d2 = Piece(Queen, d2, true)
  val P_at_d3 = Piece(Pawn, d3, true)
  val P_at_g3 = Piece(Pawn, g3, true)
  val B_at_g5 = Piece(Bishop, g5, true)
  val B_at_h3 = Piece(Bishop, h3, true)
  val R_at_d1 = Piece(Rook, d1, true)
  val R_at_f1 = Piece(Rook, f1, true)
  val K_at_c1 = Piece(King, c1, true)
  val K_at_g1 = Piece(King, g1, true)

}
