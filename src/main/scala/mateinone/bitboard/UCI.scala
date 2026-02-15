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

    if (parts(1) == "startpos") {
      board = Bitboard.initial
    } else if (parts(1) == "fen") {
      val fenParts = parts.slice(2, 8)
      board = Bitboard.fromFen(fenParts.mkString(" "))
    }

    val movesIdx = parts.indexOf("moves")
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
    var depth = 8 // Default UCI depth
    
    val depthIdx = parts.indexOf("depth")
    if (depthIdx != -1 && depthIdx < parts.length - 1) {
      depth = parts(depthIdx + 1).toInt
    }

    // Search with UCI-formatted output
    BitboardSearch.nodesSearched = 0
    BitboardSearch.ttHits = 0
    BitboardSearch.clearHistory()
    val startTime = System.nanoTime()

    for (d <- 1 to depth) {
      val score = BitboardSearch.search(board, d, -30000, 30000, 0)
      val totalDeltaMs = (System.nanoTime() - startTime) / 1000000
      val pv = BitboardSearch.getPV(board, d)
      val pvStr = pv.map(m => {
        val promoChar = mPromo(m) match {
          case Queen => "q"; case Rook => "r"; case Bishop => "b"; case Knight => "n"; case _ => ""
        }
        s"${squareName(mFrom(m))}${squareName(mTo(m))}$promoChar"
      }).mkString(" ")
      
      val scoreType = if (Math.abs(score) > 15000) "mate" else "cp"
      val scoreVal = if (scoreType == "mate") {
        val sign = if (score > 0) 1 else -1
        sign * (20000 - Math.abs(score) + 1) / 2
      } else score

      println(s"info depth $d score $scoreType $scoreVal time $totalDeltaMs nodes ${BitboardSearch.nodesSearched} pv $pvStr")
    }

    val finalPv = BitboardSearch.getPV(board, depth)
    val bestMove = if (finalPv.nonEmpty) finalPv.head else MoveGen.generateMoves(board).head
    val bestPromoChar = mPromo(bestMove) match {
      case Queen => "q"; case Rook => "r"; case Bishop => "b"; case Knight => "n"; case _ => ""
    }
    println(s"bestmove ${squareName(mFrom(bestMove))}${squareName(mTo(bestMove))}$bestPromoChar")
  }
}
