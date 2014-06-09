package mateinone

object File { // TODO make value class
  val files = Vector.range(0, 8)
  private val fileIter = files.iterator
  val A, B, C, D, E, F, G, H = fileIter.next()
}

object Rank { // TODO make value class
  val ranks = Vector.range(0, 8)
  private val rankIter = ranks.iterator
  val _1, _2, _3, _4, _5, _6, _7, _8 = rankIter.next()
}

object Square {
  val squares: Vector[Vector[Square]] = File.files.map(f => Rank.ranks.map(r => Square(f, r)))
  val outside: Square = Square(-1, -1)
  private val squareIter = squares.flatten.toIterator
  val A1, A2, A3, A4, A5, A6, A7, A8,
      B1, B2, B3, B4, B5, B6, B7, B8,
      C1, C2, C3, C4, C5, C6, C7, C8,
      D1, D2, D3, D4, D5, D6, D7, D8,
      E1, E2, E3, E4, E5, E6, E7, E8,
      F1, F2, F3, F4, F5, F6, F7, F8,
      G1, G2, G3, G4, G5, G6, G7, G8,
      H1, H2, H3, H4, H5, H6, H7, H8 = squareIter.next()
  def square(file: Int, rank: Int): Square = if (file > 7 || file < 0 || rank > 7 || rank < 0) outside else squares(file)(rank)
  private val fileStrings = Vector("a", "b", "c", "d", "e", "f", "g", "h")
}
case class Square private(file: Int, rank: Int) {
  def +(rhs: (Int, Int)): Square = Square.square(file + rhs._1, rank + rhs._2)
  override def toString: String = Square.fileStrings(file) + (rank + 1).toString
}
