package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import java.io._

class IntegrationSpec extends Specification {
  // The examples share global state (TranspositionTable, System.in), so run them one at a time.
  sequential

  "Engine Integration" should {
    "run a short simulation via Main" in {
      // Run with depth 1 for speed
      Main.main(Array("--depth", "1"))
      success
    }

    "handle full UCI protocol commands" in {
      val commands = Seq(
        "uci",
        "isready",
        "ucinewgame",
        "position startpos moves e2e4 e7e5 g1f3",
        "go depth 2",
        "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves g1f3",
        "go depth 1",
        "quit"
      ).mkString("", "\n", "\n")

      // UCI.loop reads System.in. Its println writes to Console.out, which
      // System.setOut does not redirect once Console has been used.
      val originalIn = System.in
      val outputBuffer = new ByteArrayOutputStream()
      System.setIn(new ByteArrayInputStream(commands.getBytes("UTF-8")))
      try scala.Console.withOut(outputBuffer)(UCI.loop())
      finally System.setIn(originalIn)

      val output = outputBuffer.toString
      output must contain("uciok")
      output must contain("readyok")
      output must contain("bestmove")
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
