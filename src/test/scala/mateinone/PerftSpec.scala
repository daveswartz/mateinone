package mateinone

import org.specs2.mutable._
import mateinone.bitboard._

// Reference counts from https://www.chessprogramming.org/Perft_Results
class PerftSpec extends Specification {

  def perft(b: Bitboard, depth: Int): Long = {
    if (depth == 0) return 1L
    val moves = MoveGen.generateMoves(b)
    var nodes = 0L
    var i = 0
    while (i < moves.length) {
      val m = moves(i)
      b.makeMove(m)
      if (!LegalChecker.isInCheck(b, b.sideToMove ^ 1)) nodes += perft(b, depth - 1)
      b.unmakeMove(m)
      i += 1
    }
    nodes
  }

  def counts(fen: String, depth: Int): List[Long] =
    (1 to depth).map(d => perft(Bitboard.fromFen(fen), d)).toList

  "Perft" should {

    "match the start position" in {
      counts("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 5) must beEqualTo(
        List(20L, 400L, 8902L, 197281L, 4865609L))
    }

    "match Kiwipete" in {
      counts("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 4) must beEqualTo(
        List(48L, 2039L, 97862L, 4085603L))
    }

    "match position 3" in {
      counts("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 5) must beEqualTo(
        List(14L, 191L, 2812L, 43238L, 674624L))
    }

    "match position 4" in {
      counts("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 4) must beEqualTo(
        List(6L, 264L, 9467L, 422333L))
    }

    "match position 5" in {
      counts("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 4) must beEqualTo(
        List(44L, 1486L, 62379L, 2103487L))
    }

    "match position 6" in {
      counts("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 4) must beEqualTo(
        List(46L, 2079L, 89890L, 3894594L))
    }
  }
}
