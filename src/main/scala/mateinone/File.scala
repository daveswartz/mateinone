package mateinone

object File {
  def allFiles: List[File] = List(A, B, C, D, E, F, G, H)
  def fromInt(n: Int): Option[File] = n match {
    case 1 => Some(A)
    case 2 => Some(B)
    case 3 => Some(C)
    case 4 => Some(D)
    case 5 => Some(E)
    case 6 => Some(F)
    case 7 => Some(G)
    case 8 => Some(H)
    case _ => None
  }
  def offset(f: File, o: Int): Option[File] = fromInt(f.n + o)
  def inc(f: File): Option[File] = offset(f, 1)
  def identity(f: File): Option[File] = Some(f)
  def dec(f: File): Option[File] = offset(f, -1)
}
sealed class File(val n: Int) extends Ordered[File] {
  def compare(that: File): Int = this.n.compare(that.n)
}
case object A extends File(1)
case object B extends File(2)
case object C extends File(3)
case object D extends File(4)
case object E extends File(5)
case object F extends File(6)
case object G extends File(7)
case object H extends File(8)
