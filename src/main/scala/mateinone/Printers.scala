package mateinone

import scala.language.implicitConversions
import Square._

object Printers {
  implicit def pieceToPrintablePiece(p: Piece) = new {
    def print = p match {
      case Piece(White, Pawn, _) => "♙"
      case Piece(White, Rook, _) => "♖"
      case Piece(White, Knight, _) => "♘"
      case Piece(White, Bishop, _) => "♗"
      case Piece(White, Queen, _) => "♕"
      case Piece(White, King, _) => "♔"
      case Piece(Black, Pawn, _) => "♟"
      case Piece(Black, Rook, _) => "♜"
      case Piece(Black, Knight, _) => "♞"
      case Piece(Black, Bishop, _) => "♝"
      case Piece(Black, Queen, _) => "♛"
      case Piece(Black, King, _) => "♚"
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
          case Some(Piece(White, Pawn, _)) => " ♙ |"
          case Some(Piece(White, Rook, _)) => " ♖ |"
          case Some(Piece(White, Knight, _)) => " ♘ |"
          case Some(Piece(White, Bishop, _)) => " ♗ |"
          case Some(Piece(White, Queen, _)) => " ♕ |"
          case Some(Piece(White, King, _)) => " ♔ |"
          case Some(Piece(Black, Pawn, _)) => " ♟ |"
          case Some(Piece(Black, Rook, _)) => " ♜ |"
          case Some(Piece(Black, Knight, _)) => " ♞ |"
          case Some(Piece(Black, Bishop, _)) => " ♝ |"
          case Some(Piece(Black, Queen, _)) => " ♛ |"
          case Some(Piece(Black, King, _)) => " ♚ |"
          case None => "   |"
        }).zip(8.to(1, -1)).map { case (rank, i) => rank.reduce(_+_)+" **"+i+"**" }.reduce(_+"\n"+_)
    }
  }
}
