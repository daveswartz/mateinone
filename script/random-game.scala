import mateinone._
import scala.annotation.tailrec
import scala.util.Random

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
    case None => "∙"
  }).zip(8.to(1, -1)).map { case (rank, i) => rank.reduce(_+_)+" "+i }.reduce(_+"\n"+_)+"\nabcdefgh"

def writeMove(move: Move): String = {

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

  def writeSquare(square: Square): String = writeFile(square.file) + writeRank(square.rank)

  def writePieceType(pieceType: PieceType): String = pieceType match {
    case Pawn => "♙"
    case Rook => "♖"
    case Knight => "♘"
    case Bishop => "♗"
    case Queen => "♕"
    case King => "♔"
  }

  move match {
    case SimpleMove(start, end) =>
      writeSquare(start)+"->"+writeSquare(end)
    case Promotion(start, end, promotionType) =>
      writeSquare(start)+"->"+writeSquare(end)+"="+writePieceType(promotionType)
    case Castle.whiteKingside =>
      "O-O"
    case Castle.blackKingside =>
      "O-O"
    case Castle.whiteQueenside =>
      "O-O-O"
    case Castle.blackQueenside =>
      "O-O-O"
  }

}

@tailrec def printAndMove(index: Int, board: Board, lastMoves: Seq[Move], doMove: Board => Move) {

  def doPrint() = {
    val moveIndex = index/2
    def printlnBoard() { println(writeBoard(board)) }
    lastMoves match {
      case Seq() =>
        printlnBoard()
      case Seq(p1) =>
        println(moveIndex+". "+writeMove(p1))
        printlnBoard()
      case Seq(p1, p2) =>
        println(moveIndex+". "+writeMove(p1)+" "+writeMove(p2))
        printlnBoard()
    }
  }

  doPrint()
  val nextMove = doMove(board)

  printAndMove(
    index + 1,
    board.move(nextMove).get,
    lastMoves match {
      case Seq() => Seq(nextMove)
      case Seq(lastMove) => Seq(lastMove, nextMove)
      case Seq(_, _) => Seq(nextMove)
    },
    doMove
  )

}

def randomMove(board: Board): Move = { val moves = board.moves; moves.toList(Random.nextInt(moves.size)) }
printAndMove(1, Board(), Seq(), randomMove)
