import mateinone._
import BoardWriter._
import scala.annotation.tailrec
import scala.util.Random

def randomMove(board: Board): Move = board.moves.toList(Random.nextInt(board.moves.size))

var index = 1
@tailrec def printAndMove(board: Board, nextMove: Board => Move) {
  println(writeBoard(board))
  val next = nextMove(board)
  println(index+": "+writeMove(next))
  val moved = board.move(next)
  index = index + 1
  printAndMove(moved.get, nextMove)
}

printAndMove(Board(), randomMove)

// TODO Add promotion!
// TODO Add castling!
// TODO Add black!
// TODO Add captures!
// TODO Add scores!
