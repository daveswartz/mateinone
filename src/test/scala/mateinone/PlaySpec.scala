package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import java.io.{ByteArrayOutputStream, StringReader}

class PlaySpec extends Specification {
  // play searches through the global TranspositionTable, so run the examples one at a time.
  sequential

  // Plays from fen at depth 1, answering the prompts with input, and returns everything printed.
  def play(fen: String, input: String*): String = {
    val out = new ByteArrayOutputStream()
    Console.withIn(new StringReader(input.mkString("", "\n", "\n"))) {
      Console.withOut(out)(Main.play(Bitboard.fromFen(fen), 1))
    }
    out.toString
  }

  "Human vs computer play" should {
    "suggest a legal piece and destination in its prompts" in {
      val out = play("4k3/8/8/8/8/8/8/R3K3 w - - 0 1", "a1", "zz", "q")
      out must contain("Choose piece to move [a1, 'q' to quit]: ")
      out must contain("Select destination [1-10 or a2]: ")
    }
  }
}
