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
  private val strings = Vector("a", "b", "c", "d", "e", "f", "g", "h")
}
case class File private(n: Int) extends Line[File] {
  override protected def lineOption = File.files.lift
  override def toString: String = File.strings(n)
}

object Rank {
  val ranks: Vector[Rank] = Vector.range(0, 8).map(Rank.apply)
  private val rankIter = ranks.iterator
  val _1, _2, _3, _4, _5, _6, _7, _8 = rankIter.next()
}
case class Rank private(n: Int) extends Line[Rank] {
  override protected def lineOption = Rank.ranks.lift
  override def toString: String = (n + 1).toString
}

object Square {
  val squares: Vector[Vector[Square]] = File.files.map(f => Rank.ranks.map(r => Square(f, r)))
  private val squareIter = squares.flatten.iterator
  val a1, a2, a3, a4, a5, a6, a7, a8,
      b1, b2, b3, b4, b5, b6, b7, b8,
      c1, c2, c3, c4, c5, c6, c7, c8,
      d1, d2, d3, d4, d5, d6, d7, d8,
      e1, e2, e3, e4, e5, e6, e7, e8,
      f1, f2, f3, f4, f5, f6, f7, f8,
      g1, g2, g3, g4, g5, g6, g7, g8,
      h1, h2, h3, h4, h5, h6, h7, h8 = squareIter.next()
  def square(file: File, rank: Rank): Square = squares(file.n)(rank.n)
}
case class Square private(file: File, rank: Rank) {
  def +(rhs: (Int, Int)): Option[Square] = (file + rhs._1).flatMap(fo => (rank + rhs._2).map(ro => Square.square(fo, ro)))
  def -(rhs: Square): (Int, Int) = (file - rhs.file, rank - rhs.rank)
  override def toString: String = file.toString + rank.toString
}
