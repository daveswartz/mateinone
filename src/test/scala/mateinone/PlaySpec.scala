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

    "name the piece when the computer promotes" in {
      val out = play("k7/8/8/8/8/8/p7/7K b - - 0 1", "q")
      out must contain("pv a2a1q")
      out must contain("Computer played: a2a1q")
    }

    "let the human choose the promotion piece" in {
      val fen = "8/P6k/8/8/8/8/8/K7 w - - 0 1"
      val out = play(fen, "a7", "a8n", "q")
      out must contain("Destinations for a7: 1. a8q, 2. a8r, 3. a8b, 4. a8n")
      out must contain("Select destination [1-4 or a8q]: ")
      out must contain("Knights : a8")
      play(fen, "a7", "a8", "q") must contain("Queens  : a8")
    }

    "show the evaluation from White's side on both sides' turns" in {
      // White is a queen up. Black (the computer) moves first, then the human.
      val out = play("k7/8/8/8/8/8/8/KQ6 b - - 0 1", "q")
      out must contain("Current Evaluation: +")
      out must not(contain("Current Evaluation: -"))
    }
  }
}
