package mateinone

import org.specs2.mutable._
import mateinone.Board.OccupiedPath

class BoardSpec extends Specification {
  "OccupiedPath" should {
    "vacate occupied squares" in {
      val path = List(Square(F, `1`), Square(G, `2`), Square(H, `3`))
      val occupiedPath = OccupiedPath(path, Set(Square(G, `2`)))
      occupiedPath.vacate(Square(G, `2`)) must beEqualTo(OccupiedPath(path, Set()))

    }
  }

  "Board" should {
    "have the correct state after construction" in {
      val board = Board()
      board.pieces must haveSize(16)
      board.moves must haveSize(20)
    }
    "allow Nf3" in {
      Board().move(SimpleMove(Piece(Knight, Square(G, `1`)), Square(F, `3`))) must
        beSome.which(_.pieces.exists(p => p.square == Square(F, `3`) && p.pieceType == Knight))
    }
    "allow g3" in {
      Board().move(SimpleMove(Piece(Pawn, Square(G, `2`)), Square(G, `3`))) must
        beSome.which(_.pieces.exists(p => p.square == Square(G, `3`) && p.pieceType == Pawn))
    }
    "allow Bh3" in {
      Board().move(SimpleMove(Piece(Pawn, Square(G, `2`)), Square(G, `3`)))
        .flatMap(_.move(SimpleMove(Piece(Bishop, Square(F, `1`)), Square(H, `3`)))) must
          beSome.which { board =>
            board.pieces.exists(p => p.square == Square(H, `3`) && p.pieceType == Bishop)
          }
    }
    "allow O-O" in {
      Board().move(SimpleMove(Piece(Knight, Square(G, `1`)), Square(F, `3`)))
        .flatMap(_.move(SimpleMove(Piece(Pawn, Square(G, `2`)), Square(G, `3`))))
        .flatMap(_.move(SimpleMove(Piece(Bishop, Square(F, `1`)), Square(H, `3`))))
        .flatMap(_.move(KingsideCastle)) must
          beSome.which { board =>
            board.pieces.exists(p => p.square == Square(G, `1`) && p.pieceType == King) &&
              board.pieces.exists(p => p.square == Square(F, `1`) && p.pieceType == Rook)
          }
    }
  }
}
