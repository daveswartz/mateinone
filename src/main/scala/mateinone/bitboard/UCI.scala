package mateinone.bitboard

import Constants._
import mateinone.TranspositionTable
import java.util.Scanner

object UCI {
  private var board = Bitboard.initial

  def loop(): Unit = {
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
        board = Bitboard.initial
        TranspositionTable.clear()
      } else if (line.startsWith("position")) {
        parsePosition(line)
      } else if (line.startsWith("go")) {
        parseGo(line)
      } else if (line == "quit") {
        return
      } else if (line == "stop") {
        // TODO: Implement search interruption
      }
    }
  }

  private def parsePosition(line: String): Unit = {
    val parts = line.split(" ")
    if (parts.length < 2) return
    val movesIdx = parts.indexOf("moves")

    if (parts(1) == "startpos") {
      board = Bitboard.initial
    } else if (parts(1) == "fen") {
      // The FEN runs up to "moves", since it can leave out its move counters.
      val fenParts = parts.slice(2, if (movesIdx == -1) parts.length else movesIdx)
      board = Bitboard.fromFen(fenParts.mkString(" "))
    }

    if (movesIdx != -1) {
      for (i <- movesIdx + 1 until parts.length) {
        val moveStr = parts(i)
        val legalMoves = MoveGen.generateMoves(board)
        legalMoves.find(m => {
          val promoChar = mPromo(m) match {
            case Queen => "q"; case Rook => "r"; case Bishop => "b"; case Knight => "n"; case _ => ""
          }
          s"${squareName(mFrom(m))}${squareName(mTo(m))}$promoChar" == moveStr.toLowerCase
        }) match {
          case Some(m) => board.makeMove(m)
          case None => // Ignore invalid moves
        }
      }
    }
  }

  private def parseGo(line: String): Unit = {
    val parts = line.split(" ")
    // The number after the named token, e.g. 100 for "movetime 100".
    def value(name: String): Option[Long] = parts.indexOf(name) match {
      case i if i >= 0 && i < parts.length - 1 => parts(i + 1).toLongOption
      case _ => None
    }
    val movetime = value("movetime")
    // With a time limit, as deep as the time allows; with neither a limit nor a depth, depth 8.
    val depth = value("depth").map(_.toInt).getOrElse(if (movetime.isDefined) BitboardSearch.MaxDepth else 8)

    // Search with UCI-formatted output
    BitboardSearch.nodesSearched = 0
    BitboardSearch.ttHits = 0
    BitboardSearch.clearHistory()
    // A stored score can depend on the moves that led to its position, as a repeat's does, so
    // an earlier go's results don't hold for this one.
    TranspositionTable.clear()
    val startTime = System.nanoTime()
    val deadline = movetime.fold(Long.MaxValue)(ms => startTime + ms * 1000000)
    BitboardSearch.stopped = false

    var bestMove = 0
    var d = 1
    while (d <= depth && !BitboardSearch.stopped) {
      // Depth 1 runs without the clock, so there's a move to play when the time runs out.
      BitboardSearch.deadline = if (d == 1) Long.MaxValue else deadline
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
    // So that a later search without a clock runs to its depth.
    BitboardSearch.deadline = Long.MaxValue
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
