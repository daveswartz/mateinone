package mateinone

sealed trait Line[T <: Line[T]] extends Ordered[T] {
  val n: Int
  protected def lineOption: Int => Option[T]
  def +(rhs: Int): Option[T] = lineOption(n + rhs)
  def -(rhs: T): Int = n - rhs.n
  def compare(that: T): Int = n.compare(that.n)
}

object File {
  private val strings = Vector("a", "b", "c", "d", "e", "f", "g", "h")
  val files = 0.until(8).toVector.map(File(_))
  val Vector(a, b, c, d, e, f, g, h) = files
}
case class File private(n: Int) extends Line[File] {
  import File._
  override protected def lineOption = files.lift
  override def toString: String = strings(n)
}

object Rank {
  val ranks: Vector[Rank] = 0.until(8).toVector.map(Rank(_))
  val Vector(_1, _2, _3, _4, _5, _6, _7, _8) = ranks
}
case class Rank private(n: Int) extends Line[Rank] {
  import Rank._
  override protected def lineOption = ranks.lift
  override def toString: String = (n + 1).toString
}

object Square {
  import File._
  import Rank._
  private def fileSquares(f: File) = (Square(f,_1), Square(f,_2), Square(f,_3), Square(f,_4), Square(f,_5), Square(f,_6), Square(f,_7), Square(f,_8))
  val (a1, a2, a3, a4, a5, a6, a7, a8) = fileSquares(a)
  val (b1, b2, b3, b4, b5, b6, b7, b8) = fileSquares(b)
  val (c1, c2, c3, c4, c5, c6, c7, c8) = fileSquares(c)
  val (d1, d2, d3, d4, d5, d6, d7, d8) = fileSquares(d)
  val (e1, e2, e3, e4, e5, e6, e7, e8) = fileSquares(e)
  val (f1, f2, f3, f4, f5, f6, f7, f8) = fileSquares(f)
  val (g1, g2, g3, g4, g5, g6, g7, g8) = fileSquares(g)
  val (h1, h2, h3, h4, h5, h6, h7, h8) = fileSquares(h)
  private val squares: Vector[Square] = Vector(a1, a2, a3, a4, a5, a6, a7, a8,
                                               b1, b2, b3, b4, b5, b6, b7, b8,
                                               c1, c2, c3, c4, c5, c6, c7, c8,
                                               d1, d2, d3, d4, d5, d6, d7, d8,
                                               e1, e2, e3, e4, e5, e6, e7, e8,
                                               f1, f2, f3, f4, f5, f6, f7, f8,
                                               g1, g2, g3, g4, g5, g6, g7, g8,
                                               h1, h2, h3, h4, h5, h6, h7, h8)
  def square(file: File, rank: Rank): Square = squares(file.n * 8 + rank.n)
}
case class Square private(file: File, rank: Rank) {
  import Square._
  def +(rhs: (Int, Int)): Option[Square] = (file + rhs._1).flatMap(fo => (rank + rhs._2).map(ro => square(fo, ro)))
  def -(rhs: Square): (Int, Int) = (file - rhs.file, rank - rhs.rank)
  override def toString: String = file.toString + rank.toString
}
