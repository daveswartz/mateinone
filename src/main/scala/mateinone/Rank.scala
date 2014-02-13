package mateinone

object Rank {
  val all: Vector[Rank] = 0.to(7).toVector.map(Rank(_))
  val Vector(_1, _2, _3, _4, _5, _6, _7, _8) = all
  def fromInt = all.lift
}
case class Rank private(n: Int) extends Ordered[Rank] {
  def +(rhs: Int): Option[Rank] = Rank.fromInt(n + rhs)
  def -(rhs: Rank): Int = n - rhs.n
  override def toString: String = n.toString
  def compare(that: Rank): Int = n.compare(that.n)
}
