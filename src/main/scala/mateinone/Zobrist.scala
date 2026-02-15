package mateinone

import java.util.Random

object Zobrist {
  private val seed = 0xDEADC0DE
  private val random = new Random(seed)

  // [Color: 0=White, 1=Black][PieceType: 0=P, 1=N, 2=B, 3=R, 4=Q, 5=K][Square: 0-63]
  val pieces: Array[Array[Array[Long]]] = Array.fill(2, 6, 64)(random.nextLong())

  val sideToMove: Long = random.nextLong()

  // 0=WhiteKingside, 1=WhiteQueenside, 2=BlackKingside, 3=BlackQueenside
  val castling: Array[Long] = Array.fill(4)(random.nextLong())

  // 0-7 for files A-H
  val enPassant: Array[Long] = Array.fill(8)(random.nextLong())
}
