package mateinone

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

  private object MarkdownMove {
    private val fileStrings = Vector("a", "b", "c", "d", "e", "f", "g", "h")
  }
  class MarkdownMove(m: Move) {
    import MarkdownMove._
    private def print(s: Square): String = fileStrings(s.file.n) + (s.rank.n + 1).toString
    def print(p: PromotionType): String = p match {
      case Rook => "♖"
      case Knight => "♘"
      case Bishop => "♗"
      case Queen => "♕"
    }
    def print: String = m match {
      case s: SimpleMove => print(s.start)+"->"+print(s.end)
      case p: Promotion => print(p.start)+"->"+print(p.end)+"="+print(p.promotionType)
      case c: Castle => if (c == `O-O`) "O-O" else "O-O-O"
    }
  }
  implicit def moveToMarkdownMove(m: Move) = new MarkdownMove(m)

}
