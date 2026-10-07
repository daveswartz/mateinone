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
      println("MateInOne: human vs computer")
      println(s"Search depth: $depth")
      play(Bitboard.initial, depth)
    } else {
      println("MateInOne: self-play")
      println(s"Search depth: $depth")
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
      println(f"Evaluation: ${BitboardSearch.formatScore(currentEval)}")

      if (b.isThreefoldRepetition) { println("Threefold repetition. Draw."); return }
      val moves = MoveGen.generateMoves(b).filter(m => {
        b.makeMove(m)
        val legal = !LegalChecker.isInCheck(b, b.sideToMove ^ 1)
        b.unmakeMove(m)
        legal
      })
      if (moves.isEmpty) {
        // The human plays White.
        if (!LegalChecker.isInCheck(b, b.sideToMove)) println("Stalemate. Draw.")
        else if (b.sideToMove == White) println("Checkmate. You lose.")
        else println("Checkmate. You win.")
        return
      }

      if (b.sideToMove == White) {
        val movableSquares = moves.map(mFrom).distinct.sortBy(sq => (b.pieceAt(sq), squareName(sq)))
        
        println("\nYour pieces with legal moves:")
        val pieceNames = Array("Pawns", "Knights", "Bishops", "Rooks", "Queens", "Kings")
        // Number the pieces straight through the groups, in the order they're listed.
        val numberedSquares = movableSquares.zipWithIndex
        for (pt <- 0 to 5) {
          val pieceSqs = numberedSquares.filter { case (sq, _) => b.pieceAt(sq) == pt }
          if (pieceSqs.nonEmpty) {
            val names = pieceSqs.map { case (sq, i) => s"${i + 1}. ${squareName(sq)}" }.mkString(", ")
            println(f"${pieceNames(pt) + ":"}%-9s$names")
          }
        }

        println()
        val fromInput = ask(s"Choose a piece [${numberRange(movableSquares.length)}, a square such as ${squareName(movableSquares.head)}, or q to quit]: ")
        if (isQuit(fromInput)) return

        val fromSq = choose(fromInput, movableSquares, squareName)
        val pieceMoves = moves.filter(m => fromSq.contains(mFrom(m)))

        if (pieceMoves.isEmpty) {
          println(notAChoice(fromInput))
        } else {
          val sortedPieceMoves = pieceMoves.sortBy(m => squareName(mTo(m)))
          val destinations = sortedPieceMoves.zipWithIndex.map { case (m, i) =>
            s"${i + 1}. ${destName(m)}"
          }
          println(s"Destinations for ${squareName(mFrom(pieceMoves.head))}: ${destinations.mkString(", ")}")
          
          val toInput = ask(s"Choose a destination [${numberRange(sortedPieceMoves.length)}, a square such as ${squareName(mTo(sortedPieceMoves.head))}, or q to quit]: ")
          if (isQuit(toInput)) return

          // A bare square picks its first move, which for a promotion is the queen.
          val selectedMove = choose(toInput, sortedPieceMoves, destName)
            .orElse(sortedPieceMoves.find(m => squareName(mTo(m)) == toInput))

          selectedMove match {
            case Some(m) =>
              b.makeMove(m)
              println(s"You played: ${moveName(m)}")
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

  // Prints the prompt and reads an answer, asking again after a blank line.
  private def ask(prompt: String): String = {
    print(prompt)
    val input = scala.io.StdIn.readLine()
    if (input != null && input.trim.isEmpty) ask(prompt) else input
  }

  // null is end of input.
  private def isQuit(input: String): Boolean = input == null || input == "q" || input == "quit"

  private def notAChoice(input: String): String = s">>> $input is not one of the choices. Try again."

  private def numberRange(n: Int): String = if (n == 1) "1" else s"1-$n"

  // A number picks by its position in the list; anything else is matched against the names.
  private def choose[A](input: String, choices: Seq[A], name: A => String): Option[A] =
    input.toIntOption match {
      case Some(n) => choices.lift(n - 1)
      case None => choices.find(c => name(c) == input)
    }

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
      // Take the move from the search: the table's entry for this position can hold an older,
      // deeper search's move, or another position's entry. The rest of the PV comes from the table.
      if (BitboardSearch.rootBestMove != 0) bestMove = BitboardSearch.rootBestMove
      val pv = if (bestMove == 0) Nil else {
        b.makeMove(bestMove)
        val rest = BitboardSearch.getPV(b, d - 1)
        b.unmakeMove(bestMove)
        bestMove :: rest
      }
      
      val pvStr = pv.map(moveName).mkString(" ")
      val nps = if (totalDelta > 0) (BitboardSearch.nodesSearched / totalDelta).toLong else 0
      
      println(f"depth $d%2d score ${BitboardSearch.formatScore(lastScore)}%s time $totalDelta%.2fs nodes ${BitboardSearch.nodesSearched}%,d nps $nps%,d pv $pvStr")
    }
    
    if (bestMove == 0) MoveGen.generateMoves(b).head else bestMove
  }

  def step(b: Bitboard, depth: Int, n: Int): Unit = {
    if (b.isThreefoldRepetition) {
      println("Threefold repetition. Draw.")
      return
    }

    val moves = MoveGen.generateMoves(b)
    val inCheck = LegalChecker.isInCheck(b, b.sideToMove)
    
    if (moves.isEmpty) {
      if (inCheck) println(s"Checkmate. ${if (b.sideToMove == White) "Black" else "White"} wins.")
      else println("Stalemate. Draw.")
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
