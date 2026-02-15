package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import java.io._

class IntegrationSpec extends Specification {

  "Engine Integration" should {
    "run a short simulation via Main" in {
      // Run with depth 1 for speed
      Main.main(Array("--depth", "1"))
      success
    }

    "handle full UCI protocol commands" in {
      val originalIn = System.in
      val originalOut = System.out
      
      val in = new PipedInputStream()
      val out = new PipedOutputStream(in)
      val writer = new PrintWriter(out)
      
      val outputBuffer = new ByteArrayOutputStream()
      System.setOut(new PrintStream(outputBuffer))
      System.setIn(in)
      
      val uciThread = new Thread(new Runnable {
        def run(): Unit = UCI.loop()
      })
      uciThread.start()
      
      writer.println("uci")
      writer.println("isready")
      writer.println("ucinewgame")
      writer.println("position startpos moves e2e4 e7e5 g1f3")
      writer.println("go depth 2")
      writer.println("position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves g1f3")
      writer.println("go depth 1")
      writer.println("quit")
      writer.flush()
      
      uciThread.join(5000)
      
      val output = outputBuffer.toString
      System.setIn(originalIn)
      System.setOut(originalOut)
      
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
