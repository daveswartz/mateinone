package mateinone

import scala.language.implicitConversions

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
}
