package mateinone.bitboard

object Constants {
  // Square Indices (Little-Endian Rank-File Mapping)
  // A1=0, B1=1 ... H1=7, A2=8 ... H8=63
  final val A1 = 0; final val B1 = 1; final val C1 = 2; final val D1 = 3; final val E1 = 4; final val F1 = 5; final val G1 = 6; final val H1 = 7
  final val A8 = 56; final val B8 = 57; final val C8 = 58; final val D8 = 59; final val E8 = 60; final val F8 = 61; final val G8 = 62; final val H8 = 63
  final val SquareNone = 64

  // Colors
  final val White = 0
  final val Black = 1
  final val ColorNone = 2

  // Piece Types
  final val Pawn = 0
  final val Knight = 1
  final val Bishop = 2
  final val Rook = 3
  final val Queen = 4
  final val King = 5
  final val PieceNone = 6

  // Bitboard Masks
  final val FileA = 0x0101010101010101L
  final val FileB = FileA << 1
  final val FileC = FileA << 2
  final val FileD = FileA << 3
  final val FileE = FileA << 4
  final val FileF = FileA << 5
  final val FileG = FileA << 6
  final val FileH = FileA << 7

  final val Rank1 = 0xFFL
  final val Rank2 = Rank1 << 8
  final val Rank3 = Rank1 << 16
  final val Rank4 = Rank1 << 24
  final val Rank5 = Rank1 << 32
  final val Rank6 = Rank1 << 40
  final val Rank7 = Rank1 << 48
  final val Rank8 = Rank1 << 56

  // Castling Rights (4 bits: WK, WQ, BK, BQ)
  final val CastleWK = 1
  final val CastleWQ = 2
  final val CastleBK = 4
  final val CastleBQ = 8
  final val CastleAll = 15

  // Utility
  def squareIndex(file: Int, rank: Int): Int = (rank * 8) + file
  def fileOf(sq: Int): Int = sq & 7
  def rankOf(sq: Int): Int = sq >> 3
  
  def squareName(sq: Int): String = {
    val f = fileOf(sq)
    val r = rankOf(sq)
    s"${('a' + f).toChar}${r + 1}"
  }
  
  def printBitboard(bb: Long): String = {
    val sb = new StringBuilder()
    for (r <- 7 to 0 by -1) {
      for (f <- 0 to 7) {
        val sq = squareIndex(f, r)
        if (((bb >> sq) & 1) != 0) sb.append("X ") else sb.append(". ")
      }
      sb.append("\n")
    }
    sb.toString()
  }
}
