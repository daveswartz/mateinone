package mateinone

import mateinone.bitboard._
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

      if (b.sideToMove == Constants.White) {
        val movableSquares = moves.map(_.from).distinct.sortBy(sq => (b.pieceAt(sq), Constants.squareName(sq)))
        
        println("\nYour pieces with legal moves:")
        val pieceNames = Array("Pawns", "Knights", "Bishops", "Rooks", "Queens", "Kings")
        for (pt <- 0 to 5) {
          val pieceSqs = movableSquares.filter(sq => b.pieceAt(sq) == pt)
          if (pieceSqs.nonEmpty) {
            val names = pieceSqs.map(Constants.squareName).mkString(", ")
            println(f"${pieceNames(pt)}%-8s: $names")
          }
        }

        print(s"\nChoose piece to move [e2, 'q' to quit]: ")
        val fromInput = scala.io.StdIn.readLine()
        if (fromInput == null || fromInput == "q" || fromInput == "quit") return
        
        val pieceMoves = moves.filter(m => Constants.squareName(m.from) == fromInput)
        
        if (pieceMoves.isEmpty) {
          println(s">>> No legal moves for piece at '$fromInput'. Try again.")
        } else {
          val sortedPieceMoves = pieceMoves.sortBy(m => Constants.squareName(m.to))
          val destinations = sortedPieceMoves.zipWithIndex.map { case (m, i) => 
            s"${i + 1}. ${Constants.squareName(m.to)}" 
          }
          println(s"Destinations for $fromInput: ${destinations.mkString(", ")}")
          
          print(s"Select destination [Number or e4]: ")
          val toInput = scala.io.StdIn.readLine()
          
          val selectedMove = if (toInput.nonEmpty && toInput.forall(_.isDigit)) {
            val idx = toInput.toInt - 1
            if (idx >= 0 && idx < sortedPieceMoves.length) Some(sortedPieceMoves(idx)) else None
          } else {
            sortedPieceMoves.find(m => Constants.squareName(m.to) == toInput)
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
        println(s"Computer played: ${Constants.squareName(m.from)}${Constants.squareName(m.to)}")
      }
    }
  }

  private def parseMove(input: String, legalMoves: List[Move]): Option[Move] = {
    if (input.length < 4) return None
    val fromStr = input.substring(0, 2)
    val toStr = input.substring(2, 4)
    
    legalMoves.find(m => 
      Constants.squareName(m.from) == fromStr && Constants.squareName(m.to) == toStr
    )
  }

  private def findBestMove(b: Bitboard, depth: Int): Move = {
    BitboardSearch.nodesSearched = 0
    BitboardSearch.ttHits = 0
    val startTime = System.nanoTime()
    
    var bestMove: Option[Move] = None
    var lastScore = 0

    for (d <- 1 to depth) {
      val iterStart = System.nanoTime()
      lastScore = BitboardSearch.search(b, d, -30000, 30000, 0)
      val iterDelta = (System.nanoTime() - iterStart) / 1e9
      val totalDelta = (System.nanoTime() - startTime) / 1e9
      
      val pv = BitboardSearch.getPV(b, d)
      bestMove = pv.headOption
      
      val pvStr = pv.map(m => s"${Constants.squareName(m.from)}${Constants.squareName(m.to)}").mkString(" ")
      val nps = if (totalDelta > 0) (BitboardSearch.nodesSearched / totalDelta).toLong else 0
      
      println(f"depth $d%2d score ${BitboardSearch.formatScore(lastScore)}%s time $totalDelta%.2fs nodes ${BitboardSearch.nodesSearched}%,d nps $nps%,d pv $pvStr")
    }
    
    bestMove.getOrElse(MoveGen.generateMoves(b).head)
  }

  def step(b: Bitboard, depth: Int, n: Int): Unit = {
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

    println(s"\nMove ${n/2 + 1} (${if (b.sideToMove == Constants.White) "White" else "Black"}) thinking...")
    val m = findBestMove(b, depth)
    
    b.makeMove(m)
    println(b.print(m))
    println("-" * 10)

    step(b, depth, n + 1)
  }
}
