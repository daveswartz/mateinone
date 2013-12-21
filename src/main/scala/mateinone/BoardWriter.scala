package mateinone

object BoardWriter {

  private def writePieceType(pieceType: PieceType): String = pieceType match {
    case Pawn => "♙"
    case Rook => "♖"
    case Knight => "♘"
    case Bishop => "♗"
    case Queen => "♕"
    case King => "♔"
  }

  def writeBoard(board: Board): String = Square.fileRank
    .map(_.reverse).transpose
    .map(_.map(board.piece))
    .map(_.map {
      case Some(piece) =>
        writePieceType(piece.pieceType)
      case None =>
        " "
    })
    .map(_.reduce(_+_)).reduce(_+"\n"+_)

  private def writeSquare(square: Square): String = {
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

  def writeMove(move: Move): String = move match {
    case SimpleMove(piece, end) =>
      writePieceType(piece.pieceType)+"->"+writeSquare(end)
    case Promotion(piece, end, promotionType) =>
      writePieceType(piece.pieceType)+"->"+writeSquare(end)+"="+writePieceType(promotionType)
    case KingsideCastle =>
      "O-O"
    case QueensideCastle =>
      "O-O-O"
  }

}
