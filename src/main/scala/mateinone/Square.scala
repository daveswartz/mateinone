package mateinone

object Square {

  private def forFile(file: File): List[Square] = Rank.allRanks.map(Square(file, _))

  val aFile = forFile(A)
  val bFile = forFile(B)
  val cFile = forFile(C)
  val dFile = forFile(D)
  val eFile = forFile(E)
  val fFile = forFile(F)
  val gFile = forFile(G)
  val hFile = forFile(H)

  val List(a1, a2, a3, a4, a5, a6, a7, a8) = aFile
  val List(b1, b2, b3, b4, b5, b6, b7, b8) = bFile
  val List(c1, c2, c3, c4, c5, c6, c7, c8) = cFile
  val List(d1, d2, d3, d4, d5, d6, d7, d8) = dFile
  val List(e1, e2, e3, e4, e5, e6, e7, e8) = eFile
  val List(f1, f2, f3, f4, f5, f6, f7, f8) = fFile
  val List(g1, g2, g3, g4, g5, g6, g7, g8) = gFile
  val List(h1, h2, h3, h4, h5, h6, h7, h8) = hFile

  val fileRank: List[List[Square]] = List(aFile, bFile, cFile, dFile, eFile, fFile, gFile, hFile)

  private val all: Vector[Square] = fileRank.flatten.toVector

  def get(file: File, rank: Rank): Square = all((file.n - 1) * 8 + rank.n - 1)

  def offset(s: Square, f: Int, r: Int): Option[Square] =
    File.offset(s.file, f).flatMap(fo => Rank.offset(s.rank, r).map(ro => Square.get(fo, ro)))

  def offset(a: Square, b: Square): (Int, Int) = (File.offset(a.file, b.file), Rank.offset(a.rank, b.rank))

}

case class Square private(file: File, rank: Rank)
