import mateinone._
import GithubFlavoredMarkdownPrinter._
import scala.annotation.tailrec
import scala.util.Random

@tailrec def printAndMove(index: Int, board: Board, last: Seq[MoveBase]) {
  if (index > 1) println(index/2+". "+last.map(_.print).fold("")(_+" "+_))
  println(board.print)
  if (board.isCheckmate) println(board.turn.other.toString+" wins!")
  else if (board.isStalemate) println("Stalemate.")
  else {
    val next = board.moves.toList(Random.nextInt(board.moves.size))
    printAndMove(index + 1, board.move(next).get, if (last.size == 2) Seq(next) else last :+ next)
  }
}
printAndMove(1, Board.initial, Seq())
