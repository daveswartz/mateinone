package mateinone

import mateinone.bitboard._
import mateinone.bitboard.Constants._
import mateinone.TerminalPrinter._

object Main {
  def main(args: Array[String]): Unit = {
    if (args.contains("--uci")) {
      UCI.loop()
      return
    }

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
      // evaluate scores from the side to move; show it from White's side so it doesn't flip each turn.
      val sideToMoveEval = BitboardEvaluator.evaluate(b, 0)
      val currentEval = if (b.sideToMove == White) sideToMoveEval else -sideToMoveEval
      println(f"Current Evaluation: ${BitboardSearch.formatScore(currentEval)}")

      if (b.isThreefoldRepetition) { println("Draw by threefold repetition"); return }
      val moves = MoveGen.generateMoves(b).filter(m => {
        b.makeMove(m)
        val legal = !LegalChecker.isInCheck(b, b.sideToMove ^ 1)
        b.unmakeMove(m)
        legal
      })
      if (moves.isEmpty) {
        // The human plays White.
        if (!LegalChecker.isInCheck(b, b.sideToMove)) println("Stalemate!")
        else if (b.sideToMove == White) println("Checkmate! You lose.")
        else println("Checkmate! You win.")
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

        print(s"\nChoose a piece [a square such as ${squareName(movableSquares.head)} or q to quit]: ")
        val fromInput = scala.io.StdIn.readLine()
        if (isQuit(fromInput)) return

        val pieceMoves = moves.filter(m => squareName(mFrom(m)) == fromInput)

        if (pieceMoves.isEmpty) {
          println(notAChoice(fromInput))
        } else {
          val sortedPieceMoves = pieceMoves.sortBy(m => squareName(mTo(m)))
          val destinations = sortedPieceMoves.zipWithIndex.map { case (m, i) =>
            s"${i + 1}. ${destName(m)}"
          }
          println(s"Destinations for $fromInput: ${destinations.mkString(", ")}")
          
          val numbers = if (sortedPieceMoves.length == 1) "1" else s"1-${sortedPieceMoves.length}"
          print(s"Choose a destination [$numbers, a square such as ${squareName(mTo(sortedPieceMoves.head))}, or q to quit]: ")
          val toInput = scala.io.StdIn.readLine()
          if (isQuit(toInput)) return

          val selectedMove = if (toInput != null && toInput.nonEmpty && toInput.forall(_.isDigit)) {
            val idx = toInput.toInt - 1
            if (idx >= 0 && idx < sortedPieceMoves.length) Some(sortedPieceMoves(idx)) else None
          } else {
            // A bare square picks its first move, which for a promotion is the queen.
            sortedPieceMoves.find(m => destName(m) == toInput)
              .orElse(sortedPieceMoves.find(m => squareName(mTo(m)) == toInput))
          }

          selectedMove match {
            case Some(m) => b.makeMove(m)
            case None => println(notAChoice(toInput))
          }
        }
      } else {
        println("Computer is thinking...")
        val m = findBestMove(b, depth)
        b.makeMove(m)
        println(s"Computer played: ${moveName(m)}")
      }
    }
  }

  // null is end of input.
  private def isQuit(input: String): Boolean = input == null || input == "q" || input == "quit"

  private def notAChoice(input: String): String = s">>> $input is not one of the choices. Try again."

  // The promotion letter in UCI coordinate notation, e.g. "q" in a7a8q.
  private def promoSuffix(m: Int): String = mPromo(m) match {
    case Queen => "q"; case Rook => "r"; case Bishop => "b"; case Knight => "n"; case _ => ""
  }

  private def destName(m: Int): String = s"${squareName(mTo(m))}${promoSuffix(m)}"

  private def moveName(m: Int): String = s"${squareName(mFrom(m))}${destName(m)}"

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
      
      val pvStr = pv.map(moveName).mkString(" ")
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
