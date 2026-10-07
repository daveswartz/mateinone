package mateinone

import org.specs2.mutable._
import mateinone.bitboard._
import mateinone.bitboard.Constants._
import java.io.{ByteArrayOutputStream, StringReader}

class PlaySpec extends Specification {
  // play searches through the global TranspositionTable, so run the examples one at a time.
  sequential

  // Plays from fen at depth 1, answering the prompts with input, and returns everything printed.
  def play(fen: String, input: String*): String = play(Bitboard.fromFen(fen), input: _*)

  def play(b: Bitboard, input: String*): String = {
    val out = new ByteArrayOutputStream()
    Console.withIn(new StringReader(input.mkString("", "\n", "\n"))) {
      Console.withOut(out)(Main.play(b, 1))
    }
    out.toString
  }

  // The starting position after the given moves, e.g. "g1f3".
  def afterMoves(moves: String*): Bitboard = {
    val b = Bitboard.initial
    for (move <- moves)
      b.makeMove(MoveGen.generateMoves(b).find(m => squareName(mFrom(m)) + squareName(mTo(m)) == move).get)
    b
  }

  // The starting position, reached for the third time.
  def threefold: Bitboard = afterMoves("g1f3", "g8f6", "f3g1", "f6g8", "g1f3", "g8f6", "f3g1", "f6g8")

  "Human vs computer play" should {
    "suggest a legal piece and destination in its prompts" in {
      val out = play("4k3/8/8/8/8/8/8/R3K3 w - - 0 1", "a1", "zz", "q")
      out must contain("Choose a piece [1-2, a square such as a1, or q to quit]: ")
      out must contain("Choose a destination [1-10, a square such as a2, or q to quit]: ")
    }

    "accept a number at the piece prompt" in {
      val out = play("4k3/8/8/8/8/8/8/R3K3 w - - 0 1", "2", "q")
      out must contain("Rooks:   1. a1")
      out must contain("Kings:   2. e1")
      out must contain("Destinations for e1: 1. d1, 2. d2, 3. e2, 4. f1, 5. f2")
    }

    "answer a bad choice the same way at both prompts" in {
      val out = play("4k3/8/8/8/8/8/8/R3K3 w - - 0 1", "zz", "a1", "yy", "q")
      out must contain(">>> zz is not one of the choices. Try again.")
      out must contain(">>> yy is not one of the choices. Try again.")
    }

    "answer a number too big for an Int as a bad choice" in {
      val out = play("4k3/8/8/8/8/8/8/R3K3 w - - 0 1", "a1", "99999999999", "q")
      out must contain(">>> 99999999999 is not one of the choices. Try again.")
    }

    "ask again after a blank answer" in {
      val out = play("4k3/8/8/8/8/8/8/R3K3 w - - 0 1", "", "a1", "", "q")
      out must not(contain(">>>"))
      "Your pieces with legal moves".r.findAllIn(out).length must beEqualTo(1)
      "Choose a piece".r.findAllIn(out).length must beEqualTo(2)
      "Choose a destination".r.findAllIn(out).length must beEqualTo(2)
    }

    "quit with q at the destination prompt" in {
      val out = play("4k3/8/8/8/8/8/8/R3K3 w - - 0 1", "a1", "q")
      "Your pieces with legal moves".r.findAllIn(out).length must beEqualTo(1)
    }

    "announce a win when the human mates the computer" in {
      // Ra8# is a back-rank mate.
      val out = play("6k1/5ppp/8/8/8/8/8/R5K1 w - - 0 1", "a1", "a8")
      out must contain("Checkmate. You win.")
      out must not(contain("You lose"))
    }

    "announce a loss when the human is mated" in {
      play("6k1/8/8/8/8/8/5PPP/r5K1 w - - 0 1") must contain("Checkmate. You lose.")
    }

    "announce a draw by stalemate" in {
      // Black's king has no legal move and isn't in check.
      play("7k/7P/6K1/8/8/8/8/8 b - - 0 1") must contain("Stalemate. Draw.")
    }

    "announce a draw by threefold repetition" in {
      play(threefold) must contain("Threefold repetition. Draw.")
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
      out must contain("Choose a destination [1-4, a square such as a8, or q to quit]: ")
      out must contain("Knights: 1. a8")
      play(fen, "a7", "a8", "q") must contain("Queens:  1. a8")
    }

    "show the evaluation from White's side on both sides' turns" in {
      // White is a queen up. Black (the computer) moves first, then the human.
      val out = play("k7/8/8/8/8/8/8/KQ6 b - - 0 1", "q")
      "(?m)^Evaluation: \\+".r.findFirstIn(out) must beSome
      out must not(contain("Evaluation: -"))
    }

    "start with a header in the same form as self-play's" in {
      val out = new ByteArrayOutputStream()
      Console.withIn(new StringReader("q\n")) {
        Console.withOut(out)(Main.main(Array("--play", "--depth", "1")))
      }
      out.toString must startWith("MateInOne: human vs computer\nSearch depth: 1\n")
    }
  }

  "Self-play" should {
    "announce a draw by threefold repetition the same way as play" in {
      val out = new ByteArrayOutputStream()
      Console.withOut(out)(Main.step(threefold, 1, 0))
      out.toString must contain("Threefold repetition. Draw.")
    }
  }
}
