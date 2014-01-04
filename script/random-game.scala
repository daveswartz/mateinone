import mateinone._
import BoardWriter._
import scala.annotation.tailrec
import scala.util.Random

def randomPly(board: Board): Move = board.moves.toList(Random.nextInt(board.moves.size))

def print(plyIndex: Int, board: Board, plysForMove: Seq[Move]) = {
  def printlnBoard() { println(writeBoard(board)) }
  def boardIndex = plyIndex/2
  plysForMove match {
    case Seq() =>
      printlnBoard()
    case Seq(p1) =>
      println(boardIndex+". "+writeMove(p1))
      printlnBoard()
    case Seq(p1, p2) =>
      println(boardIndex+". "+writeMove(p1)+" "+writeMove(p2))
      printlnBoard()
  }
}

@tailrec def printAndMove(plyIndex: Int, board: Board, plysForMove: Seq[Move], nextPly: Board => Move) {
  print(plyIndex, board, plysForMove)
  val next = nextPly(board)
  val after = board.move(next)
  val nextPlysForMoves = plysForMove match {
    case Seq() => Seq(next)
    case Seq(last) => Seq(last, next)
    case Seq(_, _) => Seq(next)
  }
  printAndMove(plyIndex + 1, after.get, nextPlysForMoves, nextPly)
}

printAndMove(1, Board(), Seq(), randomPly)
