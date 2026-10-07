package mateinone.bitboard

import Constants._
import mateinone.TranspositionTable
import java.util.Scanner
import scala.util.control.NonFatal

object UCI {
  // The tests set it directly, to search a board that position would reject.
  private[mateinone] var board = Bitboard.initial

  // The search runs on its own thread, so that the loop can read stop while it runs.
  private var searchThread: Option[Thread] = None
  // Set by stop on the loop's thread, and read by the search's.
  @volatile private var stopRequested = false

  // Returns the exit code: 0 after quit or the end of the input, 1 after a position the engine
  // can't play.
  def loop(): Int = {
    val scanner = new Scanner(System.in)
    while (scanner.hasNextLine) {
      val line = scanner.nextLine().trim
      if (line == "uci") {
        println("id name MateInOne")
        println("id author Dave Swartz & Gemini")
        println("uciok")
      } else if (line == "isready") {
        println("readyok")
      } else if (line == "ucinewgame") {
        awaitSearch()
        board = Bitboard.initial
        TranspositionTable.clear()
      } else if (line.startsWith("position")) {
        awaitSearch()
        parsePosition(line) match {
          // The engine can't play from a position it can't make sense of, so it says why and quits,
          // as Stockfish does.
          case Some(reason) =>
            println(s"info string invalid position: $reason")
            return 1
          case None =>
        }
      } else if (line.startsWith("go")) {
        awaitSearch()
        parseGo(line)
      } else if (line == "quit") {
        stopSearch()
        return 0
      } else if (line == "stop") {
        stopSearch()
      }
    }
    // The end of the input is a quit, as in Stockfish.
    stopSearch()
    0
  }

  // Waits for the search to end. The GUI shouldn't send position or go while one runs, but if it
  // does, they wait.
  private def awaitSearch(): Unit = {
    searchThread.foreach(_.join())
    searchThread = None
  }

  // Stops the search, which answers with bestmove, and waits for it.
  private def stopSearch(): Unit = {
    stopRequested = true
    awaitSearch()
  }

  // Sets the board, and returns why the position can't be played, if it can't.
  private def parsePosition(line: String): Option[String] = {
    val parts = line.split(" ")
    if (parts.length < 2) return None
    val movesIdx = parts.indexOf("moves")

    if (parts(1) == "startpos") {
      board = Bitboard.initial
    } else if (parts(1) == "fen") {
      // The FEN runs up to "moves", since it can leave out its move counters.
      val fenParts = parts.slice(2, if (movesIdx == -1) parts.length else movesIdx)
      board = Bitboard.fromFen(fenParts.mkString(" "))
      // The search needs one king a side, as Stockfish's does.
      val kings = (side: Int) => java.lang.Long.bitCount(board.pieceBB(side)(King))
      if (kings(White) != 1 || kings(Black) != 1) return Some("each side needs exactly one king")
    }

    // A move that isn't legal would leave the board out of step with the GUI's game, as in Stockfish.
    if (movesIdx != -1) {
      var i = movesIdx + 1
      while (i < parts.length) {
        MoveGen.generateMoves(board).find(m => moveName(m) == parts(i).toLowerCase && isLegal(m)) match {
          case Some(m) => board.makeMove(m)
          case None => return Some(s"illegal move ${parts(i)}")
        }
        i += 1
      }
    }
    None
  }

  // Whether the move leaves the mover's king out of check.
  private def isLegal(m: Int): Boolean = {
    board.makeMove(m)
    val legal = !LegalChecker.isInCheck(board, board.sideToMove ^ 1)
    board.unmakeMove(m)
    legal
  }

  private def parseGo(line: String): Unit = {
    val parts = line.split(" ")
    // The number after the named token, e.g. 100 for "movetime 100".
    def value(name: String): Option[Long] = parts.indexOf(name) match {
      case i if i >= 0 && i < parts.length - 1 => parts(i + 1).toLongOption
      case _ => None
    }
    // The side to move's clock, with 30 moves to go if the GUI doesn't say.
    val (time, inc) = if (board.sideToMove == White) ("wtime", "winc") else ("btime", "binc")
    val clockTime = value(time).map(BitboardSearch.timeForMove(_, value(inc).getOrElse(0L), value("movestogo").getOrElse(30L)))
    val timeLimit = (value("movetime") ++ clockTime).minOption
    val infinite = parts.contains("infinite")
    // With a time limit or infinite, as deep as it can; with neither, nor a depth, depth 8.
    val depth = value("depth").map(_.toInt).getOrElse(if (timeLimit.isDefined || infinite) BitboardSearch.MaxDepth else 8)

    stopRequested = false
    val thread = new Thread(() => search(depth, timeLimit, infinite), "search")
    thread.start()
    searchThread = Some(thread)
  }

  // Searches the board with UCI-formatted output, until the depth, the time limit or a stop.
  private def search(depth: Int, timeLimit: Option[Long], infinite: Boolean): Unit = {
    BitboardSearch.nodesSearched = 0
    BitboardSearch.ttHits = 0
    BitboardSearch.clearHistory()
    // A stored score can depend on the moves that led to its position, as a repeat's does, so
    // an earlier go's results don't hold for this one.
    TranspositionTable.clear()
    val startTime = System.nanoTime()
    val deadline = timeLimit.fold(Long.MaxValue)(ms => startTime + ms * 1000000)
    val stop = () => stopRequested || System.nanoTime() >= deadline
    BitboardSearch.stopped = false

    var bestMove = 0
    var d = 1
    try {
      // Depth 1 runs to the end, so there's a move to play when the search stops.
      while (d <= depth && (d == 1 || !stop())) {
        BitboardSearch.shouldStop = if (d == 1) () => false else stop
        val score = BitboardSearch.search(board, d, -30000, 30000, 0)
        if (!BitboardSearch.stopped) {
          val totalDeltaMs = (System.nanoTime() - startTime) / 1000000
          // Take the move from the search, as play does. The rest of the PV comes from the table.
          bestMove = BitboardSearch.rootBestMove
          val pv = if (bestMove == 0) Nil else {
            board.makeMove(bestMove)
            val rest = BitboardSearch.getPV(board, d - 1)
            board.unmakeMove(bestMove)
            bestMove :: rest
          }
          val pvStr = pv.map(moveName).mkString(" ")
      
          val scoreType = if (Math.abs(score) > 15000) "mate" else "cp"
          val scoreVal = if (scoreType == "mate") {
            val sign = if (score > 0) 1 else -1
            sign * (20000 - Math.abs(score) + 1) / 2
          } else score

          println(s"info depth $d score $scoreType $scoreVal time $totalDeltaMs nodes ${BitboardSearch.nodesSearched} pv $pvStr")
        }
        d += 1
      }
    } catch {
      // A failed search mustn't leave the GUI waiting for bestmove, so it gets the last finished
      // depth's move, and an info string saying why it went no deeper.
      case NonFatal(e) =>
        println(s"info string search failed: $e")
        e.printStackTrace(Console.err)
    }
    // An infinite search answers only at stop, even once it's as deep as it goes.
    while (infinite && !stopRequested) Thread.sleep(1)
    // So that a later search without a clock runs to its depth.
    BitboardSearch.shouldStop = () => false
    BitboardSearch.stopped = false

    // 0000 is UCI's null move, for a position with no legal move.
    println(s"bestmove ${if (bestMove == 0) "0000" else moveName(bestMove)}")
  }

  private def moveName(m: Int): String = {
    val promoChar = mPromo(m) match {
      case Queen => "q"; case Rook => "r"; case Bishop => "b"; case Knight => "n"; case _ => ""
    }
    s"${squareName(mFrom(m))}${squareName(mTo(m))}$promoChar"
  }
}
