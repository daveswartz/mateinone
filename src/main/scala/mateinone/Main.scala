package mateinone

import mateinone.bitboard._
import mateinone.bitboard.Constants._
import mateinone.TerminalPrinter._

object Main {
  def main(args: Array[String]): Unit = {
    if (args.contains("--uci")) sys.exit(UCI.loop())

    val depthArg = args.indexOf("--depth") match {
      case i if i >= 0 && i < args.length - 1 => Some(args(i + 1).toInt)
      case _ => None
    }

    val playMode = args.contains("--play")

    // A time control such as 5+3: 5 minutes each, and 3 seconds more after each move.
    val timeControl = args.indexOf("--time") match {
      case i if i >= 0 && i < args.length - 1 => Some(args(i + 1))
      case _ => None
    }
    val TimeControl = """(\d+(?:\.\d+)?)\+(\d+(?:\.\d+)?)""".r
    val clock = timeControl match {
      case None => None
      case Some(TimeControl(minutes, seconds)) =>
        Some(new ChessClock(Math.round(minutes.toDouble * 60000), Math.round(seconds.toDouble * 1000)))
      case Some(_) =>
        println("--time takes the minutes and the increment in seconds, such as 5+3.")
        return
    }

    if (playMode) {
      println("MateInOne: human vs computer")
      // With a clock and no depth, the computer searches as deep as its time allows.
      val depth = depthArg.getOrElse(if (clock.isDefined) BitboardSearch.MaxDepth else 12)
      if (depthArg.isDefined || clock.isEmpty) println(s"Search depth: $depth")
      timeControl.foreach(tc => println(s"Time control: $tc"))
      play(Bitboard.initial, depth, clock)
    } else {
      val depth = depthArg.getOrElse(12)
      println("MateInOne: self-play")
      println(s"Search depth: $depth")
      println("-" * 30)
      step(Bitboard.initial, depth, 0)
    }
  }

  def play(b: Bitboard, depth: Int, clock: Option[ChessClock] = None): Unit = {
    // Whether the side's flag has fallen on its own turn: nothing shows it until the side answers.
    def flagged(side: Int) = clock.exists(_.timeLeft(side, side) <= 0)

    while (true) {
      println("\n" + b.print)
      // evaluate scores from the side to move; show it from White's side so it doesn't flip each turn.
      val sideToMoveEval = BitboardEvaluator.evaluate(b, 0)
      val currentEval = if (b.sideToMove == White) sideToMoveEval else -sideToMoveEval
      println(f"Evaluation: ${BitboardSearch.formatScore(currentEval)}")
      for (c <- clock) {
        def left(side: Int) = ChessClock.format(c.timeLeft(side, b.sideToMove))
        println(s"Clock: You ${left(White)}, Computer ${left(Black)}")
      }

      if (b.isThreefoldRepetition) { println("Threefold repetition. Draw."); return }
      if (b.isInsufficientMaterial) { println("Insufficient material. Draw."); return }
      val moves = legalMoves(b)
      if (moves.isEmpty) {
        // The human plays White.
        if (!LegalChecker.isInCheck(b, b.sideToMove)) println("Stalemate. Draw.")
        else if (b.sideToMove == White) println("Checkmate. You lose.")
        else println("Checkmate. You win.")
        return
      }
      // After the mate check, since a mate on the last move counts over the fifty-move rule.
      if (b.isFiftyMoveRule) { println("Fifty-move rule. Draw."); return }

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
        if (flagged(White)) { println("Time forfeit. You lose."); return }
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
          if (flagged(White)) { println("Time forfeit. You lose."); return }
          if (isQuit(toInput)) return

          // A bare square picks its first move, which for a promotion is the queen.
          val selectedMove = choose(toInput, sortedPieceMoves, destName)
            .orElse(sortedPieceMoves.find(m => squareName(mTo(m)) == toInput))

          selectedMove match {
            case Some(m) =>
              b.makeMove(m)
              clock.foreach(_.moved(White))
              println(s"You played: ${moveName(m)}")
            case None => println(notAChoice(toInput))
          }
        }
      } else {
        println("Computer is thinking...")
        val timeLimit = clock.map(c => BitboardSearch.timeForMove(c.timeLeft(Black, Black), c.increment, 30))
        val m = findBestMove(b, depth, timeLimit)
        // A move made after the flag fell doesn't count.
        if (flagged(Black)) { println("Time forfeit. You win."); return }
        b.makeMove(m)
        clock.foreach(_.moved(Black))
        println(s"Computer played: ${moveName(m)}")
      }
    }
  }

  // The moves that don't leave the mover's king in check.
  private def legalMoves(b: Bitboard): Array[Int] = MoveGen.generateMoves(b).filter(m => {
    b.makeMove(m)
    val legal = !LegalChecker.isInCheck(b, b.sideToMove ^ 1)
    b.unmakeMove(m)
    legal
  })

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

  // Searches to the depth, or until the time limit in ms, if there is one.
  private def findBestMove(b: Bitboard, depth: Int, timeLimit: Option[Long] = None): Int = {
    BitboardSearch.nodesSearched = 0
    BitboardSearch.ttHits = 0
    BitboardSearch.clearHistory()
    // A stored score can depend on the moves that led to its position, as a repeat's does, so
    // an earlier move's results don't hold for this one.
    TranspositionTable.clear()
    val startTime = System.nanoTime()
    val deadline = timeLimit.fold(Long.MaxValue)(ms => startTime + ms * 1000000)
    val stop = () => System.nanoTime() >= deadline
    BitboardSearch.stopped = false
    
    var bestMove = 0
    var lastScore = 0

    var d = 1
    // Depth 1 runs to the end, so there's a move to play when the time runs out.
    while (d <= depth && (d == 1 || !stop())) {
      BitboardSearch.shouldStop = if (d == 1) () => false else stop
      var alpha = -30000
      var beta = 30000
      val windowSize = 50 
      
      if (d >= 5) {
        alpha = lastScore - windowSize
        beta = lastScore + windowSize
      }
      
      var score = BitboardSearch.search(b, d, alpha, beta, 0)
      if (!BitboardSearch.stopped && (score <= alpha || score >= beta)) {
        score = BitboardSearch.search(b, d, -30000, 30000, 0)
      }
      if (!BitboardSearch.stopped) {
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
      d += 1
    }
    // So that a later search without a clock runs to its depth.
    BitboardSearch.shouldStop = () => false
    BitboardSearch.stopped = false
    
    if (bestMove == 0) MoveGen.generateMoves(b).head else bestMove
  }

  def step(b: Bitboard, depth: Int, n: Int): Unit = {
    if (b.isThreefoldRepetition) {
      println("Threefold repetition. Draw.")
      return
    }
    if (b.isInsufficientMaterial) {
      println("Insufficient material. Draw.")
      return
    }

    val moves = legalMoves(b)
    val inCheck = LegalChecker.isInCheck(b, b.sideToMove)
    
    if (moves.isEmpty) {
      if (inCheck) println(s"Checkmate. ${if (b.sideToMove == White) "Black" else "White"} wins.")
      else println("Stalemate. Draw.")
      return
    }
    // After the mate check, since a mate on the last move counts over the fifty-move rule.
    if (b.isFiftyMoveRule) {
      println("Fifty-move rule. Draw.")
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
