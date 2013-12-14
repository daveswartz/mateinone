package mateinone

object BoardWriter {
  def writeBoard(board: Board): String = {
    def writePiece(piece: Piece): String = {
      import PieceType._
      piece.pieceType match {
        case Pawn => ""
        case Rook => "R"
        case Knight => "N"
        case Bishop => "B"
        case Queen => "Q"
        case King => "K"
      }
    } + writeSquare(piece.square)
    def rankThenFile(a: Piece, b: Piece) =
      if (a.square.rank == b.square.rank) a.square.file < b.square.file
      else if (a.square.rank > b.square.rank) true
      else false

    board.pieces.toList.sortWith(rankThenFile).map(writePiece).reduce(_ + " " + _)
  }
  def writeSquare(square: Square): String = {
    def writeFile(file: File): String = file match {
      case A => "a"
      case B => "b"
      case C => "c"
      case D => "d"
      case E => "e"
      case F => "f"
      case G => "g"
      case H => "h"
    }
    def writeRank(rank: Rank): String = rank match {
      case `1` => "1"
      case `2` => "2"
      case `3` => "3"
      case `4` => "4"
      case `5` => "5"
      case `6` => "6"
      case `7` => "7"
      case `8` => "8"
    }
    writeFile(square.file) + writeRank(square.rank)
  }
  def writeMove(move: Move): String = writeSquare(move.start) + "->" + writeSquare(move.end)
}
