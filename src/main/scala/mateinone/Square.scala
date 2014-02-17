package mateinone

sealed trait Line[T <: Line[T]] extends Ordered[T] {
  val n: Int
  protected def lineOption: Int => Option[T]
  def +(rhs: Int): Option[T] = lineOption(n + rhs)
  def -(rhs: T): Int = n - rhs.n
  def compare(that: T): Int = n.compare(that.n)
}

object File {
  val files: Vector[File] = Vector.range(0, 8).map(File.apply)
  private val fileIter = files.iterator
  val A, B, C, D, E, F, G, H = fileIter.next()
}
case class File private(n: Int) extends Line[File] {
  override protected def lineOption = File.files.lift
}

object Rank {
  val ranks: Vector[Rank] = Vector.range(0, 8).map(Rank.apply)
  private val rankIter = ranks.iterator
  val _1, _2, _3, _4, _5, _6, _7, _8 = rankIter.next()
}
case class Rank private(n: Int) extends Line[Rank] {
  override protected def lineOption = Rank.ranks.lift
}

object Square {
  val squares: Vector[Vector[Square]] = File.files.map(f => Rank.ranks.map(r => Square(f, r)))
  private val squareIter = squares.flatten.iterator
  val A1, A2, A3, A4, A5, A6, A7, A8,
      B1, B2, B3, B4, B5, B6, B7, B8,
      C1, C2, C3, C4, C5, C6, C7, C8,
      D1, D2, D3, D4, D5, D6, D7, D8,
      E1, E2, E3, E4, E5, E6, E7, E8,
      F1, F2, F3, F4, F5, F6, F7, F8,
      G1, G2, G3, G4, G5, G6, G7, G8,
      H1, H2, H3, H4, H5, H6, H7, H8 = squareIter.next()
  def square(file: File, rank: Rank): Square = squares(file.n)(rank.n)
}
case class Square private(file: File, rank: Rank) {
  def +(rhs: (Int, Int)): Option[Square] = (file + rhs._1).flatMap(fo => (rank + rhs._2).map(ro => Square.square(fo, ro)))
  def -(rhs: Square): (Int, Int) = (file - rhs.file, rank - rhs.rank)
}
