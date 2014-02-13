package mateinone

import File._

object Square {
  def forRank(r: Rank)(fs: File*): Vector[Square] = fs.map(f => Square(f, r)).toVector
  def forFile(f: File)(rs: Rank*): Vector[Square] = rs.map(r => Square(f, r)).toVector
  val Vector(a1, a2, a3, a4, a5, a6, a7, a8) = forFile(a)(Rank.all:_*)
  val Vector(b1, b2, b3, b4, b5, b6, b7, b8) = forFile(b)(Rank.all:_*)
  val Vector(c1, c2, c3, c4, c5, c6, c7, c8) = forFile(c)(Rank.all:_*)
  val Vector(d1, d2, d3, d4, d5, d6, d7, d8) = forFile(d)(Rank.all:_*)
  val Vector(e1, e2, e3, e4, e5, e6, e7, e8) = forFile(e)(Rank.all:_*)
  val Vector(f1, f2, f3, f4, f5, f6, f7, f8) = forFile(f)(Rank.all:_*)
  val Vector(g1, g2, g3, g4, g5, g6, g7, g8) = forFile(g)(Rank.all:_*)
  val Vector(h1, h2, h3, h4, h5, h6, h7, h8) = forFile(h)(Rank.all:_*)
}
case class Square(file: File, rank: Rank) {
  def +(rhs: (Int, Int)): Option[Square] = (file + rhs._1).flatMap(fo => (rank + rhs._2).map(ro => Square(fo, ro)))
  def -(rhs: Square): (Int, Int) = (file - rhs.file, rank - rhs.rank)
  override def toString: String = file.toString + rank.toString
}
