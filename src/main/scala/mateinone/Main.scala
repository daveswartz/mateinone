package mateinone

import mateinone.bitboard._
import mateinone.bitboard.Constants._
import mateinone.TerminalPrinter._

object Main {
  def main(args: Array[String]): Unit = {
    val depth = args.indexOf("--depth") match {
      case i if i >= 0 && i < args.length - 1 => args(i + 1).toInt
      case _ => 12
    }

    val playMode = args.contains("--play")

    if (playMode) {
      println("Starting MateInOne: Human vs Computer")
      println(s"Search Depth: $depth")
      play(Bitboard.initial, depth)
    } else {
      println(s"Starting MateInOne Bitboard Engine Simulation")
      println(s"Target Depth: $depth")
      println("-" * 30)
      step(Bitboard.initial, depth, 0)
    }
  }

  def play(b: Bitboard, depth: Int): Unit = {
    while (true) {
      println("\n" + b.print)
      val currentEval = BitboardEvaluator.evaluate(b, 0)
      println(f"Current Evaluation: ${BitboardSearch.formatScore(currentEval)}")

      if (b.isThreefoldRepetition) { println("Draw by threefold repetition"); return }
      val moves = MoveGen.generateMoves(b).filter(m => {
        b.makeMove(m)
        val legal = !LegalChecker.isInCheck(b, b.sideToMove ^ 1)
        b.unmakeMove(m)
        legal
      })
      if (moves.isEmpty) {
        if (LegalChecker.isInCheck(b, b.sideToMove)) println("Checkmate! You lose.")
        else println("Stalemate!")
        return
      }

      if (b.sideToMove == White) {
        val movableSquares = moves.map(mFrom).distinct.sortBy(sq => (b.pieceAt(sq), squareName(sq)))
        
        println("\nYour pieces with legal moves:")
        val pieceNames = Array("Pawns", "Knights", "Bishops", "Rooks", "Queens", "Kings")
        for (pt <- 0 to 5) {
          val pieceSqs = movableSquares.filter(sq => b.pieceAt(sq) == pt)
          if (pieceSqs.nonEmpty) {
            val names = pieceSqs.map(squareName).mkString(", ")
            println(f"${pieceNames(pt)}%-8s: $names")
          }
        }

        print(s"\nChoose piece to move [e2, 'q' to quit]: ")
        val fromInput = scala.io.StdIn.readLine()
        if (fromInput == null || fromInput == "q" || fromInput == "quit") return
        
        val pieceMoves = moves.filter(m => squareName(mFrom(m)) == fromInput)
        
        if (pieceMoves.isEmpty) {
          println(s">>> No legal moves for piece at '$fromInput'. Try again.")
        } else {
          val sortedPieceMoves = pieceMoves.sortBy(m => squareName(mTo(m)))
          val destinations = sortedPieceMoves.zipWithIndex.map { case (m, i) => 
            s"${i + 1}. ${squareName(mTo(m))}" 
          }
          println(s"Destinations for $fromInput: ${destinations.mkString(", ")}")
          
          print(s"Select destination [Number or e4]: ")
          val toInput = scala.io.StdIn.readLine()
          
          val selectedMove = if (toInput != null && toInput.nonEmpty && toInput.forall(_.isDigit)) {
            val idx = toInput.toInt - 1
            if (idx >= 0 && idx < sortedPieceMoves.length) Some(sortedPieceMoves(idx)) else None
          } else {
            sortedPieceMoves.find(m => squareName(mTo(m)) == toInput)
          }

          selectedMove match {
            case Some(m) => b.makeMove(m)
            case None => println(">>> Invalid destination. Selection cancelled.")
          }
        }
      } else {
        println("Computer is thinking...")
        val m = findBestMove(b, depth)
        b.makeMove(m)
        println(s"Computer played: ${squareName(mFrom(m))}${squareName(mTo(m))}")
      }
    }
  }

  private def findBestMove(b: Bitboard, depth: Int): Int = {
    BitboardSearch.nodesSearched = 0
    BitboardSearch.ttHits = 0
    BitboardSearch.clearHistory()
    val startTime = System.nanoTime()
    
    var bestMove = 0
    var lastScore = 0

    for (d <- 1 to depth) {
      val iterStart = System.nanoTime()
      
      var alpha = -30000
      var beta = 30000
      val windowSize = 50 
      
      if (d >= 5) {
        alpha = lastScore - windowSize
        beta = lastScore + windowSize
      }
      
      var score = BitboardSearch.search(b, d, alpha, beta, 0)
      if (score <= alpha || score >= beta) {
        score = BitboardSearch.search(b, d, -30000, 30000, 0)
      }
      lastScore = score
      
      val totalDelta = (System.nanoTime() - startTime) / 1e9
      val pv = BitboardSearch.getPV(b, d)
      if (pv.nonEmpty) bestMove = pv.head
      
      val pvStr = pv.map(m => s"${squareName(mFrom(m))}${squareName(mTo(m))}").mkString(" ")
      val nps = if (totalDelta > 0) (BitboardSearch.nodesSearched / totalDelta).toLong else 0
      
      println(f"depth $d%2d score ${BitboardSearch.formatScore(lastScore)}%s time $totalDelta%.2fs nodes ${BitboardSearch.nodesSearched}%,d nps $nps%,d pv $pvStr")
    }
    
    if (bestMove == 0) MoveGen.generateMoves(b).head else bestMove
  }

  def step(b: Bitboard, depth: Int, n: Int): Unit = {
    if (b.isThreefoldRepetition) {
      println(s"Draw by threefold repetition")
      return
    }

    val moves = MoveGen.generateMoves(b)
    val inCheck = LegalChecker.isInCheck(b, b.sideToMove)
    
    if (moves.isEmpty) {
      if (inCheck) println(s"Checkmate ${if (b.sideToMove == White) "Black" else "White"} wins")
      else println("Stalemate")
      return
    }

    println(s"\nMove ${n/2 + 1} (${if (b.sideToMove == White) "White" else "Black"}) thinking...")
    val m = findBestMove(b, depth)
    
    b.makeMove(m)
    println(b.print(m))
    println("-" * 10)

    step(b, depth, n + 1)
  }
}
