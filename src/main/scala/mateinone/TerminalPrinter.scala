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
        val white = b.turn == White
        if (s == start) "·"
        else if (b.same.pawns.contains(s)) if (white) "♙" else "♟"
        else if (b.opponent.pawns.contains(s)) if (white) "♟" else "♙"
        else if (b.same.knights.contains(s)) if (white) "♘" else "♞"
        else if (b.opponent.knights.contains(s)) if (white) "♞" else "♘"
        else if (b.same.bishops.contains(s)) if (white) "♗" else "♝"
        else if (b.opponent.bishops.contains(s)) if (white) "♝" else "♗"
        else if (b.same.rooks.contains(s)) if (white) "♖" else "♜"
        else if (b.opponent.rooks.contains(s)) if (white) "♜" else "♖"
        else if (b.same.queens.contains(s)) if (white) "♕" else "♛"
        else if (b.opponent.queens.contains(s)) if (white) "♛" else "♕"
        else if (b.same.kings.contains(s)) if (white) "♔" else "♚"
        else if (b.opponent.kings.contains(s)) if (white) "♚" else "♔"
        else " "
      }

      ("┌─────────────────┐" +:
        Squares.map(rank => rank.map(print)).map(rank => ("│"+:rank:+"│").mkString(" ")) :+
        "└─────────────────┘").mkString("\n")

    }

  }

}
