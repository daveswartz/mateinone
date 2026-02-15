package mateinone.bitboard

import Constants._

object Attacks {

  val KnightAttacks: Array[Long] = new Array[Long](64)
  val KingAttacks: Array[Long]   = new Array[Long](64)
  // PawnAttacks[Color][Square]
  val PawnAttacks: Array[Array[Long]] = Array.ofDim[Long](2, 64)

  // Initialize Tables
  initLeapers()

  private def initLeapers(): Unit = {
    for (sq <- 0 until 64) {
      val bb = 1L << sq
      val file = fileOf(sq)
      val rank = rankOf(sq)

      // Knight
      var k = 0L
      if (rank < 7 && file < 6) k |= bb << 10
      if (rank < 7 && file > 1) k |= bb << 6
      if (rank < 6 && file < 7) k |= bb << 17
      if (rank < 6 && file > 0) k |= bb << 15
      if (rank > 0 && file < 6) k |= bb >> 6
      if (rank > 0 && file > 1) k |= bb >> 10
      if (rank > 1 && file < 7) k |= bb >> 15
      if (rank > 1 && file > 0) k |= bb >> 17
      KnightAttacks(sq) = k

      // King
      var king = 0L
      // North
      if (rank < 7) {
        king |= bb << 8
        if (file > 0) king |= bb << 7
        if (file < 7) king |= bb << 9
      }
      // South
      if (rank > 0) {
        king |= bb >> 8
        if (file > 0) king |= bb >> 9
        if (file < 7) king |= bb >> 7
      }
      // East/West
      if (file > 0) king |= bb >> 1
      if (file < 7) king |= bb << 1
      KingAttacks(sq) = king

      // Pawn Attacks (Captures only)
      // White captures North-East (+9) and North-West (+7)
      var wp = 0L
      if (rank < 7) {
        if (file < 7) wp |= bb << 9
        if (file > 0) wp |= bb << 7
      }
      PawnAttacks(White)(sq) = wp

      // Black captures South-East (-7) and South-West (-9)
      var bp = 0L
      if (rank > 0) {
        if (file < 7) bp |= bb >> 7
        if (file > 0) bp |= bb >> 9
      }
      PawnAttacks(Black)(sq) = bp
    }
  }

  // Sliding Attacks (On-the-fly for now, can be optimized to Magics later)
  def bishopAttacks(sq: Int, occ: Long): Long = {
    var attacks = 0L
    // NE
    var s = sq; var f = fileOf(s); var r = rankOf(s)
    while (f < 7 && r < 7) { s += 9; attacks |= (1L << s); if (((occ >> s) & 1) != 0) { f = 8 } else { f += 1; r += 1 } }
    // NW
    s = sq; f = fileOf(s); r = rankOf(s)
    while (f > 0 && r < 7) { s += 7; attacks |= (1L << s); if (((occ >> s) & 1) != 0) { f = -1 } else { f -= 1; r += 1 } }
    // SE
    s = sq; f = fileOf(s); r = rankOf(s)
    while (f < 7 && r > 0) { s -= 7; attacks |= (1L << s); if (((occ >> s) & 1) != 0) { f = 8 } else { f += 1; r -= 1 } }
    // SW
    s = sq; f = fileOf(s); r = rankOf(s)
    while (f > 0 && r > 0) { s -= 9; attacks |= (1L << s); if (((occ >> s) & 1) != 0) { f = -1 } else { f -= 1; r -= 1 } }
    attacks
  }

  def rookAttacks(sq: Int, occ: Long): Long = {
    var attacks = 0L
    // North
    var s = sq; var r = rankOf(s)
    while (r < 7) { s += 8; attacks |= (1L << s); if (((occ >> s) & 1) != 0) { r = 8 } else { r += 1 } }
    // South
    s = sq; r = rankOf(s)
    while (r > 0) { s -= 8; attacks |= (1L << s); if (((occ >> s) & 1) != 0) { r = -1 } else { r -= 1 } }
    // East
    s = sq; var f = fileOf(s)
    while (f < 7) { s += 1; attacks |= (1L << s); if (((occ >> s) & 1) != 0) { f = 8 } else { f += 1 } }
    // West
    s = sq; f = fileOf(s)
    while (f > 0) { s -= 1; attacks |= (1L << s); if (((occ >> s) & 1) != 0) { f = -1 } else { f -= 1 } }
    attacks
  }
  
  def queenAttacks(sq: Int, occ: Long): Long = bishopAttacks(sq, occ) | rookAttacks(sq, occ)
}
