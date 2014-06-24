package mateinone

import scala.language.implicitConversions
import Square._

object TerminalPrinter {
  implicit def boardToTerminalBoard(b: Board) = new {
    def print(last: MoveBase): String = {
      val start = last match {
        case Move(s, _) => s
        case Promotion(s, _, _) => s
        case _: Castle => if (b.turn == White) E8 else E1
      }
      def print(s: Square): String = {
        val white = b.turn == White && b.same.contains(s)
        if (s == start) "·"
        else if (b.pawns.contains(s)) if (white) "♙" else "♟"
        else if (b.knights.contains(s)) if (white) "♘" else "♞"
        else if (b.bishops.contains(s)) if (white) "♗" else "♝"
        else if (b.rooks.contains(s)) if (white) "♖" else "♜"
        else if (b.queens.contains(s)) if (white) "♕" else "♛"
        else if (b.kings.contains(s)) if (white) "♔" else "♚"
        else " "
      }
      ("┌─────────────────┐" +:
        Squares.map(rank => rank.map(print)).map(rank => ("│"+:rank:+"│").mkString(" ")) :+
        "└─────────────────┘").mkString("\n")
    }
  }
}
