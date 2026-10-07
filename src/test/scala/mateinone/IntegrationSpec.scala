package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import mateinone.bitboard.Constants._
import java.io._

class IntegrationSpec extends Specification {
  // The examples share global state (TranspositionTable, System.in), so run them one at a time.
  sequential

  // Runs the UCI loop on the commands and returns everything printed.
  def uci(commands: String*): String = {
    // UCI.loop reads System.in. Its println writes to Console.out, which
    // System.setOut does not redirect once Console has been used.
    val originalIn = System.in
    val out = new ByteArrayOutputStream()
    System.setIn(new ByteArrayInputStream(commands.mkString("", "\n", "\n").getBytes("UTF-8")))
    try scala.Console.withOut(out)(UCI.loop())
    finally System.setIn(originalIn)
    out.toString
  }

  val kiwipete = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

  // The deepest iteration the info lines report.
  def lastDepth(out: String): Int = "info depth (\\d+)".r.findAllMatchIn(out).map(_.group(1).toInt).max

  "Engine Integration" should {
    "run a short simulation via Main" in {
      // Run with depth 1 for speed
      val out = new ByteArrayOutputStream()
      scala.Console.withOut(out)(Main.main(Array("--depth", "1")))
      out.toString must startWith("MateInOne: self-play\nSearch depth: 1\n")
    }

    "handle full UCI protocol commands" in {
      val output = uci(
        "uci",
        "isready",
        "ucinewgame",
        "position startpos moves e2e4 e7e5 g1f3",
        "go depth 2",
        "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves g1f3",
        "go depth 1",
        "quit"
      )
      output must contain("uciok")
      output must contain("readyok")
      output must contain("bestmove")
    }

    "search each go from an empty table" in {
      // Black takes the free queen, but an older result in the table says Kf7 wins more.
      val fen = "3qk3/8/8/8/3Q4/8/8/7K b - - 0 1"
      val b = Bitboard.fromFen(fen)
      TranspositionTable.clear()
      val kf7 = MoveGen.generateMoves(b).find(m => squareName(mFrom(m)) + squareName(mTo(m)) == "e8f7").get
      b.makeMove(kf7)
      TranspositionTable.store(b.hash, 5, -1500, TranspositionTable.Exact, None)
      uci(s"position fen $fen", "go depth 1", "quit") must contain("bestmove d8d4")
    }

    "answer bestmove 0000 when the side to move has no legal move" in {
      // Black is mated by the rook on the back rank.
      uci("position fen R5k1/5ppp/8/8/8/8/8/6K1 b - - 0 1", "go depth 2", "quit") must contain("bestmove 0000")
    }

    "apply the moves after a FEN without the move counters" in {
      // Ra8+ leaves Black only the king moves off the back rank.
      uci("position fen 4k3/8/8/8/8/8/8/R3K3 w - - moves a1a8", "go depth 1", "quit") must beMatching("(?s).*bestmove e8[def]7\n")
    }

    "stop at the movetime with the move the last finished depth found" in {
      // Depth 8 takes about 400 ms here.
      val out = uci(s"position fen $kiwipete", "go movetime 1", "quit")
      lastDepth(out) must beLessThan(8)
      out must beMatching("(?s).*bestmove [a-h][1-8][a-h][1-8]\\n")
    }

    "manage the side to move's clock" in {
      // White to move has 30 ms left, too little for more than depth 1; Black's clock doesn't count.
      val out = uci(s"position fen $kiwipete", "go wtime 30 btime 100000", "quit")
      lastDepth(out) must beLessThan(8)
      out must beMatching("(?s).*bestmove [a-h][1-8][a-h][1-8]\\n")
    }

    "handle Transposition Table collisions correctly" in {
      TranspositionTable.clear()
      val hash1 = 12345L
      val hash2 = 12345L + (1L << 20) // Same index in a 2^20 table
      
      TranspositionTable.store(hash1, 4, 100, TranspositionTable.Exact, Some(1))
      TranspositionTable.get(hash2) must beNone // Collision should return None, not incorrect entry
      
      TranspositionTable.store(hash2, 5, 200, TranspositionTable.Exact, Some(2))
      TranspositionTable.get(hash1) must beNone // New entry should replace old one at same index
      TranspositionTable.get(hash2) must beSome
    }
  }
}
