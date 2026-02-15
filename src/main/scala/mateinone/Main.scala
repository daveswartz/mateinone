package mateinone

import mateinone.bitboard._
import mateinone.TerminalPrinter._

object Main {
  def main(args: Array[String]): Unit = {
    val depth = args.indexOf("--depth") match {
      case i if i >= 0 && i < args.length - 1 => args(i + 1).toInt
      case _ => 12 // New default depth
    }

    println(s"Starting MateInOne Bitboard Engine Simulation")
    println(s"Target Depth: $depth")
    println("-" * 30)

    val b = Bitboard.initial
    step(b, depth, 0)
  }

  def step(b: Bitboard, depth: Int, n: Int): Unit = {
    // Check for game end
    if (b.isThreefoldRepetition) {
      println(s"Draw by threefold repetition")
      return
    }

    val moves = MoveGen.generateMoves(b)
    val inCheck = LegalChecker.isInCheck(b, b.sideToMove)
    
    if (moves.isEmpty) {
      if (inCheck) println(s"Checkmate ${if (b.sideToMove == Constants.White) "Black" else "White"} wins")
      else println("Stalemate")
      return
    }

    // Reset Metrics
    BitboardSearch.nodesSearched = 0
    BitboardSearch.ttHits = 0

    val start = System.nanoTime()
    // Populate TT with Iterative Deepening up to target depth
    for (d <- 1 until depth) {
      BitboardSearch.search(b, d, -30000, 30000, 0)
    }
    val score = BitboardSearch.search(b, depth, -30000, 30000, 0)
    val delta = (System.nanoTime() - start) / 1e9

    // Find the best move from TT
    val bestMoveOpt = TranspositionTable.get(b.hash).flatMap(_.bestMove).collect { case m: Move => m }
    val m = bestMoveOpt.getOrElse {
       // Fallback: just pick the first legal move if TT is weirdly empty
       moves.find(mv => {
         b.makeMove(mv)
         val legal = !LegalChecker.isInCheck(b, b.sideToMove ^ 1)
         b.unmakeMove(mv)
         legal
       }).get
    }
    
    // Apply the move
    b.makeMove(m)
    
    def isWhite(i: Int) = i % 2 == 0
    def whitePrefix(i: Int) = s"${i / 2 + 1}."
    def blackPrefix(i: Int) = s"${whitePrefix(i)} ..."
    def prefix(i: Int) = if (isWhite(i)) whitePrefix(i) else blackPrefix(i)

    println(b.print(m))
    println(s"${prefix(n)} ${Constants.squareName(m.from)}->${Constants.squareName(m.to)}")
    
    val displayScore = if (score > 15000) s"Mate in ${(20000 - score + 1) / 2}"
                       else if (score < -15000) s"Mate in ${(20000 + score + 1) / 2}"
                       else f"${score / 100.0}%+.2f"

    println(s"Score: $displayScore")
    println(f"Search time: $delta%.2fs | Nodes: ${BitboardSearch.nodesSearched}%,d | TT Hits: ${BitboardSearch.ttHits}%,d")
    println("-" * 10)

    step(b, depth, n + 1)
  }
}
