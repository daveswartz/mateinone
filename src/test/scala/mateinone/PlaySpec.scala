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

    "announce a win when the human mates the computer" in {
      // Ra8# is a back-rank mate.
      val out = play("6k1/5ppp/8/8/8/8/8/R5K1 w - - 0 1", "a1", "a8")
      out must contain("Checkmate! You win.")
      out must not(contain("You lose"))
    }

    "announce a loss when the human is mated" in {
      play("6k1/8/8/8/8/8/5PPP/r5K1 w - - 0 1") must contain("Checkmate! You lose.")
    }
  }
}
