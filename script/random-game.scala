import mateinone._
import scala.annotation.tailrec
import scala.util.Random

def writeBoard(board: Board): String = Square.fileRank
  .map(_.reverse).transpose
  .map(_.map(board.pieceAt))
  .map(_.map {
    case Some(Piece(White, Pawn, _)) => "♙"
    case Some(Piece(White, Rook, _)) => "♖"
    case Some(Piece(White, Knight, _)) => "♘"
    case Some(Piece(White, Bishop, _)) => "♗"
    case Some(Piece(White, Queen, _)) => "♕"
    case Some(Piece(White, King, _)) => "♔"
    case Some(Piece(Black, Pawn, _)) => "♟"
    case Some(Piece(Black, Rook, _)) => "♜"
    case Some(Piece(Black, Knight, _)) => "♞"
    case Some(Piece(Black, Bishop, _)) => "♝"
    case Some(Piece(Black, Queen, _)) => "♛"
    case Some(Piece(Black, King, _)) => "♚"
    case None => "∙"
  }).zip(8.to(1, -1)).map { case (rank, i) => rank.reduce(_+_)+" "+i }.reduce(_+"\n"+_)+"\nabcdefgh"

@tailrec def printAndMove(index: Int, board: Board, lastMoves: Seq[Move], doMove: Board => Move) {

  def doPrint() = {
    val moveIndex = index/2
    def printlnBoard() { println(writeBoard(board)) }
    lastMoves match {
      case Seq() =>
        printlnBoard()
      case Seq(p1) =>
        println(moveIndex+". "+p1.toString)
        printlnBoard()
      case Seq(p1, p2) =>
        println(moveIndex+". "+p1.toString+" "+p2.toString)
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
