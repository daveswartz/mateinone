package mateinone

import scala.language.implicitConversions
import Square._

object TerminalPrinter {

  implicit def boardToTerminalBoard(b: Board) = new {

    def print(last: MoveBase): String = {

      val start = last match {
        case Move(s, _) => s
        case Promotion(s, _, _) => s
        case _: Castle => if (b.same.color == White) E8 else E1
      }

      def print(s: Square): String = {
        val white = b.same.color == White
        if (s == start) "·"
        else if (b.same.squares(Pawn).contains(s)) if (white) "♙" else "♟"
        else if (b.opponent.squares(Pawn).contains(s)) if (white) "♟" else "♙"
        else if (b.same.squares(Knight).contains(s)) if (white) "♘" else "♞"
        else if (b.opponent.squares(Knight).contains(s)) if (white) "♞" else "♘"
        else if (b.same.squares(Bishop).contains(s)) if (white) "♗" else "♝"
        else if (b.opponent.squares(Bishop).contains(s)) if (white) "♝" else "♗"
        else if (b.same.squares(Rook).contains(s)) if (white) "♖" else "♜"
        else if (b.opponent.squares(Rook).contains(s)) if (white) "♜" else "♖"
        else if (b.same.squares(Queen).contains(s)) if (white) "♕" else "♛"
        else if (b.opponent.squares(Queen).contains(s)) if (white) "♛" else "♕"
        else if (b.same.squares(King).contains(s)) if (white) "♔" else "♚"
        else if (b.opponent.squares(King).contains(s)) if (white) "♚" else "♔"
        else " "
      }

      ("┌─────────────────┐" +:
       Squares.map(rank => rank.map(print)).map(rank => ("│"+:rank:+"│").mkString(" ")) :+
       "└─────────────────┘").mkString("\n")

    }

  }

}
