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

    "see a mate on the search's last ply" in {
      // Rh8 mates. At depth 1, Black's reply is quiescence's, which has to see that Black is in
      // check and has no move.
      val b = Bitboard.fromFen("k7/8/1K6/8/8/8/8/7R w - - 0 1")
      TranspositionTable.clear()
      BitboardSearch.search(b, 1, -30000, 30000, 0) must beEqualTo(20000 - 1)
    }

    "answer a check in quiescence at any ply, even past the killers' last" in {
      // Black's king has to step out of the rook's check, and quiescence can run past ply 63, the
      // last the killers hold, after a depth 64 search.
      val b = Bitboard.fromFen("4k3/8/8/8/8/8/8/4R1K1 b - - 0 1")
      TranspositionTable.clear()
      BitboardSearch.quiesce(b, -30000, 30000, 64) must beGreaterThan(-15000)
    }

    "score a zugzwang against the side to move, not as if it could pass" in {
      // Whichever side moves has to leave its pawn to the other king, so White to move loses a pawn,
      // and its score is below 0. Passing would hand the zugzwang to Black, so a null move fails high.
      val b = Bitboard.fromFen("8/8/8/3pK3/2kP4/8/8/8 w - - 0 1")
      TranspositionTable.clear()
      BitboardSearch.search(b, 5, -1, 0, 1) must beEqualTo(-1)
    }

    "not count a position from before a null move as a repeat" in {
      // The rook goes a4-a3-a4 and a4-a2-a1 while Black's king steps out and back, so the position
      // with the rook on a4 and White to move has occurred twice. After Ra4, Black's null move
      // gives that position again, but a line through a pass isn't one a game can play, so it isn't
      // a third occurrence. Black is down a rook for a knight, so its score is below 0.
      val b = Bitboard.fromFen("7k/8/8/5n2/R7/8/8/4K3 w - - 0 1")
      for (m <- Seq("a4a3", "h8g8", "a3a4", "g8h8", "a4a2", "h8g8", "a2a1", "g8h8")) b.makeMove(move(b, m))
      // Searching from here first sets the search's root here, as a game's search would.
      BitboardSearch.search(b, 1, -30000, 30000, 0)
      b.makeMove(move(b, "a1a4"))
      TranspositionTable.clear()
      BitboardSearch.search(b, 3, -1, 0, 1) must beEqualTo(-1)
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
