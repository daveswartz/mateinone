package mateinone

import scala.language.implicitConversions
import Square._

object Printers {
  implicit def pieceToPrintablePiece(p: Piece) = new {
    def print = p match {
      case Piece(White, Pawn, _, _) => "♙"
      case Piece(White, Rook, _, _) => "♖"
      case Piece(White, Knight, _, _) => "♘"
      case Piece(White, Bishop, _, _) => "♗"
      case Piece(White, Queen, _, _) => "♕"
      case Piece(White, King, _, _) => "♔"
      case Piece(Black, Pawn, _, _) => "♟"
      case Piece(Black, Rook, _, _) => "♜"
      case Piece(Black, Knight, _, _) => "♞"
      case Piece(Black, Bishop, _, _) => "♝"
      case Piece(Black, Queen, _, _) => "♛"
      case Piece(Black, King, _, _) => "♚"
    }
  }
}
import Printers._

object TerminalPrinter {
  implicit def boardToTerminalBoard(b: Board) = new {
    def print(last: Option[MoveBase]): String = {
      val start = last map {
        case Move(s, _) => s
        case Promotion(s, _, _) => s
        case _: Castle => if (b.turn == White) E8 else E1
      }
      ("┌─────────────────┐" +:
        squares.transpose.reverse.map(rank => rank.map(square => if (start == Some(square)) "·" else b.pieces.find(_.square == square).fold(" ")(_.print))).map(rank => ("│"+:rank:+"│").mkString(" ")) :+
        "└─────────────────┘").mkString("\n")
    }
  }
}

object GithubFlavoredMarkdownPrinter {
  implicit def boardToMarkdownBoard(b: Board) = new {
    def print: String = {
      " a | b | c | d | e | f | g | h | ∙\n:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:\n" +
        squares.transpose.reverse
          .map(_.map(square => b.pieces.find(_.square == square)))
          .map(_.map {
          case Some(Piece(White, Pawn, _, _)) => " ♙ |"
          case Some(Piece(White, Rook, _, _)) => " ♖ |"
          case Some(Piece(White, Knight, _, _)) => " ♘ |"
          case Some(Piece(White, Bishop, _, _)) => " ♗ |"
          case Some(Piece(White, Queen, _, _)) => " ♕ |"
          case Some(Piece(White, King, _, _)) => " ♔ |"
          case Some(Piece(Black, Pawn, _, _)) => " ♟ |"
          case Some(Piece(Black, Rook, _, _)) => " ♜ |"
          case Some(Piece(Black, Knight, _, _)) => " ♞ |"
          case Some(Piece(Black, Bishop, _, _)) => " ♝ |"
          case Some(Piece(Black, Queen, _, _)) => " ♛ |"
          case Some(Piece(Black, King, _, _)) => " ♚ |"
          case None => "   |"
        }).zip(8.to(1, -1)).map { case (rank, i) => rank.reduce(_+_)+" **"+i+"**" }.reduce(_+"\n"+_)
    }
  }
}
