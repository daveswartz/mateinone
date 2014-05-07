package mateinone

import scala.language.implicitConversions
import Square._

object Printers {
  class PrintablePiece(piece: Piece) {
    def print = piece match {
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
  implicit def pieceToPrintablePiece(p: Piece) = new PrintablePiece(p)
}
import Printers._

object TerminalPrinter {
  class TerminalBoard(b: Board) {
    private val lastMoveStart = b.history.lastOption map {
      case (Move(s, _), _) => s
      case (Promotion(s, _, _), _) => s
      case (_: Castle, _) => if (b.turn == White) E8 else E1
    }
    def print: String = ("┌─────────────────┐" +: squares.transpose.reverse.map(rank => rank.map(square => if (lastMoveStart == Some(square)) "·" else b.pieces.find(_.square == square).fold(" ")(_.print))).map(rank => ("│"+:rank:+"│").mkString(" ")) :+ "└─────────────────┘").mkString("\n")
  }
  implicit def boardToTerminalBoard(b: Board) = new TerminalBoard(b)
}

object GithubFlavoredMarkdownPrinter {
  class MarkdownBoard(b: Board) {
    def print: String = {
      " a | b | c | d | e | f | g | h | ∙\n:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:\n" +
        Square.squares.transpose.reverse
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
  implicit def boardToMarkdownBoard(b: Board) = new MarkdownBoard(b)
}

object MovePrinter {
  private object PrintableMove {
    private val fileStrings = Vector("a", "b", "c", "d", "e", "f", "g", "h")
  }
  class PrintableMove(m: MoveBase) {
    import PrintableMove._
    private def print(s: Square): String = fileStrings(s.file.n) + (s.rank.n + 1).toString
    private def print(p: PromotionType): String = p match {
      case Rook => "♖"
      case Knight => "♘"
      case Bishop => "♗"
      case Queen => "♕"
    }
    def print: String = m match {
      case s: Move => print(s.start)+"->"+print(s.end)
      case p: Promotion => print(p.start)+"->"+print(p.end)+"="+print(p.`type`)
      case c: Castle => if (c == `O-O`) "O-O" else "O-O-O"
    }
  }
  implicit def moveToPrintableMove(m: MoveBase) = new PrintableMove(m)
}