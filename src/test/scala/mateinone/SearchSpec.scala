package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import mateinone.bitboard.Constants._

class SearchSpec extends Specification {
  // The examples plant entries in the global TranspositionTable, so run them one at a time.
  sequential

  def move(b: Bitboard, name: String): Int =
    MoveGen.generateMoves(b).find(m => squareName(mFrom(m)) + squareName(mTo(m)) == name).get

  "The search" should {
    "search the root moves even when the position has a stored result" in {
      // Black mates with Ra1, but the stored result says Kf8 draws, at the depth searched.
      val b = Bitboard.fromFen("r5k1/5ppp/8/8/8/8/5PPP/6K1 b - - 0 1")
      TranspositionTable.clear()
      TranspositionTable.store(b.hash, 2, 0, TranspositionTable.Exact, Some(move(b, "g8f8")))
      BitboardSearch.search(b, 2, -30000, 30000, 0) must beGreaterThan(15000)
    }
  }
}
