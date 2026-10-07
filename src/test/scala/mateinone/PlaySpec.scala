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

  // The move named in coordinate notation without a promotion, e.g. "g1f3".
  def move(b: Bitboard, name: String): Int =
    MoveGen.generateMoves(b).find(m => squareName(mFrom(m)) + squareName(mTo(m)) == name).get

  // The starting position after the given moves.
  def afterMoves(moves: String*): Bitboard = {
    val b = Bitboard.initial
    for (name <- moves) b.makeMove(move(b, name))
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

    "show the human's move the way it shows the computer's" in {
      play("6k1/5ppp/8/8/8/8/8/R5K1 w - - 0 1", "a1", "a8") must contain("You played: a1a8")
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

    "announce a draw by insufficient material" in {
      play("4k3/8/8/8/8/8/8/4K3 w - - 0 1") must contain("Insufficient material. Draw.")
    }

    "announce a draw by the fifty-move rule" in {
      // Neither side has moved a pawn or captured for 100 half-moves.
      play("4k3/8/8/8/8/8/8/R3K3 w - - 100 80") must contain("Fifty-move rule. Draw.")
    }

    "count a mate on the hundredth half-move over the fifty-move rule" in {
      val out = play("6k1/5ppp/8/8/8/8/8/R5K1 w - - 99 80", "a1", "a8")
      out must contain("Checkmate. You win.")
      out must not(contain("Fifty-move rule"))
    }

    "name the piece when the computer promotes" in {
      val out = play("k7/8/8/8/8/8/p7/7K b - - 0 1", "q")
      out must contain("pv a2a1q")
      out must contain("Computer played: a2a1q")
    }

    "play the move the computer just searched, not an older stored one" in {
      // Black takes the free queen, but the table holds a deeper, older result that says Kf7.
      val b = Bitboard.fromFen("3qk3/8/8/8/3Q4/8/8/7K b - - 0 1")
      TranspositionTable.clear()
      TranspositionTable.store(b.hash, 5, 0, TranspositionTable.Exact, Some(move(b, "e8f7")))
      val out = play(b)
      out must contain("pv d8d4")
      out must contain("Computer played: d8d4")
    }

    "play a legal move when another position holds the root's table slot" in {
      // Black is in check from the rook, so only king moves are legal.
      val b = Bitboard.fromFen("4k3/p7/8/8/8/8/8/K3R3 b - - 0 1")
      TranspositionTable.clear()
      TranspositionTable.store(b.hash + (1L << 20), 5, 0, TranspositionTable.Exact, None)
      play(b) must contain("Computer played: e8")
    }

    "search each move from an empty table" in {
      // Black takes the free queen, but an older result in the table says Kf7 wins more.
      val b = Bitboard.fromFen("3qk3/8/8/8/3Q4/8/8/7K b - - 0 1")
      TranspositionTable.clear()
      val kf7 = move(b, "e8f7")
      b.makeMove(kf7)
      TranspositionTable.store(b.hash, 5, -1500, TranspositionTable.Exact, None)
      b.unmakeMove(kf7)
      play(b) must contain("Computer played: d8d4")
    }

    "let the human choose the promotion piece" in {
      // Black's pawn keeps K+N vs K from ending the game as insufficient material.
      val fen = "8/P6k/7p/8/8/8/8/K7 w - - 0 1"
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

    "announce a draw by insufficient material the same way as play" in {
      val out = new ByteArrayOutputStream()
      Console.withOut(out)(Main.step(Bitboard.fromFen("4k3/8/8/8/8/8/8/4K3 w - - 0 1"), 1, 0))
      out.toString must beEqualTo("Insufficient material. Draw.\n")
    }

    "announce a draw by the fifty-move rule the same way as play" in {
      val out = new ByteArrayOutputStream()
      Console.withOut(out)(Main.step(Bitboard.fromFen("4k3/8/8/8/8/8/8/R3K3 w - - 100 80"), 1, 0))
      out.toString must beEqualTo("Fifty-move rule. Draw.\n")
    }

    "end the game when the side to move has no legal move" in {
      // Black is mated by the rook on the back rank, then stalemated by the pawn and king.
      for ((fen, end) <- Seq("R5k1/5ppp/8/8/8/8/8/6K1 b - - 0 1" -> "Checkmate. White wins.\n",
                             "7k/7P/6K1/8/8/8/8/8 b - - 0 1" -> "Stalemate. Draw.\n")) yield {
        val out = new ByteArrayOutputStream()
        Console.withOut(out)(Main.step(Bitboard.fromFen(fen), 1, 0))
        out.toString must beEqualTo(end)
      }
    }
  }
}
