package mateinone

object File {
  val Files = Vector.range(0, 8)
  private val fileIter = Files.iterator
  val A, B, C, D, E, F, G, H = fileIter.next()
}

object Rank {
  val Ranks = Vector.range(0, 8)
  private val rankIter = Ranks.iterator
  val _1, _2, _3, _4, _5, _6, _7, _8 = rankIter.next()
}

object Square {
  val Squares: Vector[Vector[Square]] = Rank.Ranks.reverse.map(r => File.Files.map(f => Square(f, r)))
  val Outside: Square = Square(-1, -1)
  private val squareIter = Squares.flatten.toIterator
  val A8, B8, C8, D8, E8, F8, G8, H8,
      A7, B7, C7, D7, E7, F7, G7, H7,
      A6, B6, C6, D6, E6, F6, G6, H6,
      A5, B5, C5, D5, E5, F5, G5, H5,
      A4, B4, C4, D4, E4, F4, G4, H4,
      A3, B3, C3, D3, E3, F3, G3, H3,
      A2, B2, C2, D2, E2, F2, G2, H2,
      A1, B1, C1, D1, E1, F1, G1, H1 = squareIter.next()
  def square(file: Int, rank: Int): Square = if (file > 7 || file < 0 || rank > 7 || rank < 0) Outside else Squares(7 - rank)(file)
  private val fileStrings = Vector("a", "b", "c", "d", "e", "f", "g", "h")
}
case class Square private(file: Int, rank: Int) {
  def +(rhs: (Int, Int)): Square = Square.square(file + rhs._1, rank + rhs._2)
  override def toString: String = Square.fileStrings(file) + (rank + 1).toString
}
