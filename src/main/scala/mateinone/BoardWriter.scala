package mateinone

import Castle._

object BoardWriter {

  private def writePieceType(pieceType: PieceType): String = pieceType match { // TODO add support for black side
    case Pawn => "♙"
    case Rook => "♖"
    case Knight => "♘"
    case Bishop => "♗"
    case Queen => "♕"
    case King => "♔"
  }

  def writeBoard(board: Board): String = Square.fileRank
    .map(_.reverse).transpose
    .map(_.map(board.pieceAt))
    .map(_.map {
      case Some(Piece(White, Pawn, _, _)) => "♙"
      case Some(Piece(White, Rook, _, _)) => "♖"
      case Some(Piece(White, Knight, _, _)) => "♘"
      case Some(Piece(White, Bishop, _, _)) => "♗"
      case Some(Piece(White, Queen, _, _)) => "♕"
      case Some(Piece(White, King, _, _)) => "♔"
      case Some(Piece(Black, Pawn, _, _)) => "♟"
      case Some(Piece(Black, Rook, _, _)) => "♜"
      case Some(Piece(Black, Knight, _, _)) => "♞"
      case Some(Piece(Black, Bishop, _, _)) => "♝"
      case Some(Piece(Black, Queen, _, _)) => "♛"
      case Some(Piece(Black, King, _, _)) => "♚"
      case None =>
        "∙"
    })
    .zip(8.to(1, -1)).map { case (rank, i) => rank.reduce(_+_)+" "+i }.reduce(_+"\n"+_)+"\nabcdefgh"

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
    case SimpleMove(start, end) =>
      writeSquare(start)+"->"+writeSquare(end)
    case Promotion(start, end, promotionType) =>
      writeSquare(start)+"->"+writeSquare(end)+"="+writePieceType(promotionType)
    case `O-O` =>
      "O-O"
    case `O-O-O` =>
      "O-O-O"
  }

}
