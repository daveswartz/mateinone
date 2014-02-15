import mateinone._
import scala.annotation.tailrec
import scala.util.Random

import Square._
import File._
import Rank._
import SimpleMove._
import Castle._
import Move._

@tailrec def printAndMove(index: Int, board: Board, lastMoves: Seq[Move], doMove: Board => Move) {

  def doPrint() = {
    val moveIndex = index/2
    lastMoves match {
      case Seq() =>
        println(board)
      case Seq(p1) =>
        println(moveIndex+". "+p1.toString)
        println(board)
      case Seq(p1, p2) =>
        println(moveIndex+". "+p1.toString+" "+p2.toString)
        println(board)
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
