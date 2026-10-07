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

    "score a position that repeats since the root as a draw" in {
      // White is two rooks up, but Black checks forever: Qe1+ Kh2 Qh4+ Kg1 Qe1+ repeats at ply 5.
      val b = Bitboard.fromFen("7k/Q7/8/8/4q3/8/RR4P1/6K1 b - - 0 1")
      TranspositionTable.clear()
      BitboardSearch.search(b, 5, -30000, 30000, 0) must beEqualTo(0)
    }

    "score a stored mate by its distance from where the search meets the position" in {
      // Black's only move is Kb8, and then Rh8 mates. The first search meets the position 10 plies
      // in, as a long line does, and stores the mate; at the root it's mate in 2 plies all the same.
      val b = Bitboard.fromFen("k7/8/1K6/8/8/8/8/7R b - - 0 1")
      TranspositionTable.clear()
      BitboardSearch.search(b, 3, -30000, 30000, 10) must beEqualTo(-20000 + 12)
      BitboardSearch.search(b, 3, -30000, 30000, 0) must beEqualTo(-20000 + 2)
    }

    "spend an even share of the clock on a move, plus the increment, but not its last 50 ms" in {
      BitboardSearch.timeForMove(60000, 0, 30) must beEqualTo(2000)
      BitboardSearch.timeForMove(60000, 1000, 30) must beEqualTo(3000)
      BitboardSearch.timeForMove(60000, 0, 10) must beEqualTo(6000)
      BitboardSearch.timeForMove(100, 1000, 30) must beEqualTo(50)
      BitboardSearch.timeForMove(30, 0, 30) must beEqualTo(0)
    }

    "choose a root move even when the root has occurred three times" in {
      val b = Bitboard.initial
      for (name <- Seq("g1f3", "g8f6", "f3g1", "f6g8", "g1f3", "g8f6", "f3g1", "f6g8")) b.makeMove(move(b, name))
      TranspositionTable.clear()
      BitboardSearch.search(b, 1, -30000, 30000, 0)
      BitboardSearch.rootBestMove must not(beEqualTo(0))
    }
  }
}
