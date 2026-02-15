package mateinone

import scala.language.implicitConversions
import Square._

import bitboard._
import bitboard.Constants._

object TerminalPrinter {

  implicit class TerminalBitboard(val b: Bitboard) {
    def print: String = print(None)
    def print(last: Move): String = print(Some(last))

    private def print(last: Option[Move]): String = {
      val sb = new StringBuilder()
      val startSq = last.map(_.from).getOrElse(-1)
      
      sb.append("┌─────────────────┐\n")
      for (r <- 7 to 0 by -1) {
        sb.append("│ ")
        for (f <- 0 to 7) {
          val sq = squareIndex(f, r)
          val p = b.pieceAt(sq)
          val color = b.colorAt(sq)
          
          if (sq == startSq) {
            sb.append("· ")
          } else if (color == bitboard.Constants.ColorNone) {
            sb.append("  ")
          } else {
            val isWhite = color == bitboard.Constants.White
            val symbol = p match {
              case bitboard.Constants.Pawn => if (isWhite) "♙" else "♟"
              case bitboard.Constants.Knight => if (isWhite) "♘" else "♞"
              case bitboard.Constants.Bishop => if (isWhite) "♗" else "♝"
              case bitboard.Constants.Rook => if (isWhite) "♖" else "♜"
              case bitboard.Constants.Queen => if (isWhite) "♕" else "♛"
              case bitboard.Constants.King => if (isWhite) "♔" else "♚"
            }
            sb.append(symbol + " ")
          }
        }
        sb.append("│\n")
      }
      sb.append("└─────────────────┘")
      sb.toString()
    }
  }

  implicit class TerminalBoard(val b: mateinone.Board) {

    def print: String = print(None)
    def print(last: MoveBase): String = print(Some(last))

    private def print(last: Option[MoveBase]): String = {

      val start = last.map {
        case mateinone.Move(s, _) => s
        case mateinone.Promotion(s, _, _) => s
        case _: mateinone.Castle => if (b.same.color == mateinone.White) mateinone.Square.E8 else mateinone.Square.E1
      }

      def print(s: Square): String = {
        val white = b.same.color == mateinone.White
        if (Some(s) == start) "·"
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
