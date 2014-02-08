package mateinone

import Square._

object Move {

  implicit def moveToEither(m: Move): Either[Move, Side => Move] = Left(m)

  def toMove(e: Either[Move, Side => Move], s: Side): Move = e match { case Left(l) => l case Right(r) => r(s)}

  def moves(start: Square, end: Square): Set[_ <: Move] = {
    def default(args: (Square, Square)) = Set(SimpleMove(args._1, args._2))
    Promotion.promotion.orElse(Castle.castle).applyOrElse((start, end), default)
  }

}
sealed trait Move {
  val start: Square
  val end: Square
  def offset: (Int, Int) = Square.offset(start, end)
}

object SimpleMove {
  implicit def tupleToSimpleMove(t: (Square, Square)): SimpleMove = t match { case (start, end) => SimpleMove(start, end) }
  implicit def tupleToLeftSimpleMove(t: (Square, Square)): Either[Move, Side => Move] = t match { case (start, end) => Left(SimpleMove(start, end)) }
}
case class SimpleMove(start: Square, end: Square) extends Move {
  def promote(promotionType: PromotionType): Option[Promotion] = Promotion.promotion.lift((start, end)).flatMap(_.find(p => p.promotionType == promotionType))
  override def toString: String = start.toString+"->"+end.toString
}

object Promotion {

  private def promotions(starts: List[Square], rankOffset: Int) = for {
    start <- starts
    fileOffset <- List(-1, 0, 1)
    promotionType <- PromotionType.all
    end = start.offset(fileOffset, rankOffset)
    if end.isDefined
  } yield Promotion(start, end.get, promotionType)

  private val all = promotions(List(a7, b7, c7, d7, e7, f7, g7, h7), 1) ++ promotions(List(a2, b2, c2, d2, e2, f2, g2, h2), -1)

  val promotion = new PartialFunction[(Square, Square), Set[Promotion]] {
    override def isDefinedAt(args: (Square, Square)): Boolean = all.exists(p => args._1 == p.start && args._2 == p.end)
    override def apply(args: (Square, Square)): Set[Promotion] = PromotionType.all.map(t => Promotion(args._1, args._2, t))
  }

  implicit def promotionToLeftPromotion(p: Promotion): Either[Promotion, Side => Promotion] = Left(p)

}
case class Promotion private(start: Square, end: Square, promotionType: PromotionType) extends Move {
  override def toString: String = start.toString+"->"+end.toString+"="+promotionType.toString
}

object Castle {

  private val whiteKingside = Castle(e1, g1, SimpleMove(h1, f1))
  private val whiteQueenside = Castle(e1, c1, SimpleMove(a1, d1))
  private val blackKingside = Castle(e8, g8, SimpleMove(h8, f8))
  private val blackQueenside = Castle(e8, c8, SimpleMove(a8, d8))

  val all = Set(whiteKingside, whiteQueenside, blackKingside, blackQueenside)

  val castle = new PartialFunction[(Square, Square), Set[Castle]] {
    override def isDefinedAt(args: (Square, Square)): Boolean = all.exists(c => args._1 == c.start && args._2 == c.end)
    override def apply(args: (Square, Square)): Set[Castle] = all.filter(c => args._1 == c.start && args._2 == c.end)
  }

  val `O-O` = (_: Side) match {
    case White => whiteKingside
    case Black => blackKingside
  }

  val `O-O-O` = (_: Side) match {
    case White => whiteQueenside
    case Black => blackQueenside
  }

  implicit def castleToRightCastle(c: Side => Castle): Either[Castle, Side => Castle] = Right(c)

}
case class Castle private(start: Square, end: Square, rookMove: SimpleMove) extends Move {
  override def toString: String = if (end.file == G) "O-O" else "O-O-O"
}
