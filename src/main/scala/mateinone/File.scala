package mateinone

object File {
  val all: Vector[File] = Vector("a", "b", "c", "d", "e", "f", "g", "h").zipWithIndex.map { case (s, i) => new File(i, s) }
  val Vector(a, b, c, d, e, f, g, h) = all
  val fromInt = all.lift
}
case class File(n: Int, s: String) extends Ordered[File] {
  def +(rhs: Int): Option[File] = File.fromInt(n + rhs)
  def -(rhs: File): Int = n - rhs.n
  override def toString: String = s
  def compare(that: File): Int = n.compare(that.n)
}
