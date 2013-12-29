package mateinone

object Rank {
  def allRanks: List[Rank] = List(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`)
  def fromInt(n: Int): Option[Rank] = n match {
    case 1 => Some(`1`)
    case 2 => Some(`2`)
    case 3 => Some(`3`)
    case 4 => Some(`4`)
    case 5 => Some(`5`)
    case 6 => Some(`6`)
    case 7 => Some(`7`)
    case 8 => Some(`8`)
    case _ => None
  }
  def offset(r: Rank, o: Int): Option[Rank] = fromInt(r.n + o)
}
sealed class Rank(val n: Int) extends Ordered[Rank] {
  def compare(that: Rank): Int = this.n.compare(that.n)
}
case object `1` extends Rank(1)
case object `2` extends Rank(2)
case object `3` extends Rank(3)
case object `4` extends Rank(4)
case object `5` extends Rank(5)
case object `6` extends Rank(6)
case object `7` extends Rank(7)
case object `8` extends Rank(8)
