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

  def movesMustOnlyMove(moves: List[Move], movers: Set[Piece]) =
    Board().moves(moves) must beSome.which { board =>
      val unmoved = Board().pieces.filterNot(p => movers.map(_.pieceType).contains(p.pieceType))
      unmoved.forall(board.pieces.contains) && movers.forall(board.pieces.contains)
    }

  "Board" should {
    "have the correct state after construction" in new BoardScope {
      Board().pieces must haveSize(16)
      Board().moves must haveSize(20)
    }
    "allow knight to f3" in new BoardScope {
      movesMustOnlyMove(N_to_f3 :: Nil, Set(N_at_f3))
    }
    "allow pawn to g3" in new BoardScope {
      movesMustOnlyMove(P_to_g3 :: Nil, Set(P_at_g3))
    }
    "allow bishop to h3" in new BoardScope {
      movesMustOnlyMove(P_to_g3 :: B_to_h3 :: Nil, Set(P_at_g3, B_at_h3))
    }
    "allow kingside castle" in new BoardScope {
      movesMustOnlyMove(N_to_f3 :: P_to_g3 :: B_to_h3 :: KingsideCastle :: Nil, Set(N_at_f3, P_at_g3, B_at_h3, K_at_g1, R_at_f1))
    }
    "allow queenside castle" in new BoardScope {
      movesMustOnlyMove(N_to_a3 :: P_to_d3 :: B_to_g5 :: Q_to_d2 :: QueensideCastle :: Nil, Set(N_at_a3, P_at_d3, B_at_g5, Q_at_d2, R_at_d1, K_at_c1))
    }
  }

}

trait BoardScope extends Scope {

  val N_to_a3 = SimpleMove(Piece(Knight, b1), a3) // TODO b1 -> a3
  val N_to_f3 = SimpleMove(Piece(Knight, g1), f3)
  val Q_to_d2 = SimpleMove(Piece(Queen, d1), d2)
  val P_to_d3 = SimpleMove(Piece(Pawn, d2), d3)
  val P_to_g3 = SimpleMove(Piece(Pawn, g2), g3)
  val B_to_g5 = SimpleMove(Piece(Bishop, c1), g5)
  val B_to_h3 = SimpleMove(Piece(Bishop, f1), h3)

  val N_at_a3 = Piece(Knight, a3)
  val N_at_f3 = Piece(Knight, f3)
  val Q_at_d2 = Piece(Queen, d2)
  val P_at_d3 = Piece(Pawn, d3)
  val P_at_g3 = Piece(Pawn, g3)
  val B_at_g5 = Piece(Bishop, g5)
  val B_at_h3 = Piece(Bishop, h3)
  val R_at_d1 = Piece(Rook, d1)
  val R_at_f1 = Piece(Rook, f1)
  val K_at_c1 = Piece(King, c1)
  val K_at_g1 = Piece(King, g1)

}
