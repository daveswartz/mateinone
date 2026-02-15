package mateinone

import mateinone.evaluators.Simplified
import mateinone.TerminalPrinter._

object Main {
  def main(args: Array[String]): Unit = {
    val depth = args.indexOf("--depth") match {
      case i if i >= 0 && i < args.length - 1 => args(i + 1).toInt
      case _ => 8 // Default depth
    }

    println(s"Starting MateInOne Engine Simulation")
    println(s"Search Depth: $depth")
    println(s"Evaluator: Simplified")
    println("-" * 30)

    step(Board.initial, depth, 0)
  }

  def step(board: Board, depth: Int, n: Int): Unit = {
    if (board.isCheckmate) println(s"Checkmate ${board.opponent.color.toString} wins")
    else if (board.isStalemate) println("Stalemate")
    else if (board.isInsufficientMaterial) println("Insufficient mating material")
    else if (board.isThreefoldRepetition) println(s"${board.same.color.toString} claimed draw by threefold repetition")
    else if (board.isFiftyMoveRule) println(s"${board.same.color.toString} claimed draw by fifty-move rule")
    else {
      val start = System.nanoTime()
      val searchScore = Search.nextMove(board, depth, Simplified)
      val delta = (System.nanoTime() - start) / 1e9

      def isWhite(i: Int) = i % 2 == 0
      def whitePrefix(i: Int) = s"${i / 2 + 1}."
      def blackPrefix(i: Int) = s"${whitePrefix(i)} ..."
      def prefix(i: Int) = if (isWhite(i)) whitePrefix(i) else blackPrefix(i)
      def prefixIfWhite(i: Int) = if (isWhite(i)) whitePrefix(i) else ""

      val afterNextMoves = {
        val indices = (n + 1).to(n + 1 + depth)
        val prefixes = prefix(indices.head) +: indices.tail.map(prefixIfWhite)
        searchScore.moves.tail.zip(prefixes).map { case (m, p) => s"$p $m" }.mkString(" ")
      }

      val nextMove = searchScore.moves.head
      val nextBoard = board.move(nextMove).get
      var centiPawns = searchScore.score / 100d
      if (!isWhite(n)) centiPawns *= -1

      println(nextBoard.print(nextMove))
      println(s"${prefix(n)} $nextMove")
      println(f"Score: $centiPawns%+.2f $afterNextMoves")
      println(f"Search time: $delta%.2fs | Nodes: ${Search.nodesSearched}%,d | TT Hits: ${Search.ttHits}%,d")

      step(nextBoard, depth, n + 1)
    }
  }
}
