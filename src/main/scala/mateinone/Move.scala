package mateinone

import Square._
import File._
import Rank._

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
  def offset: (Int, Int) = start - end
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
    end = start + (fileOffset, rankOffset)
    if end.isDefined
  } yield Promotion(start, end.get, promotionType)

  private val all = promotions(forRank(_7)(a, b, c, d, e, f, g, h).toList, 1) ++ promotions(forRank(_2)(a, b, c, d, e, f, g, h).toList, -1)

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

  private val whiteKingside = Castle(Square(e, _1), Square(g, _1), SimpleMove(Square(h, _1), Square(f, _1)))
  private val whiteQueenside = Castle(Square(e, _1), Square(c, _1), SimpleMove(Square(a, _1), Square(d, _1)))
  private val blackKingside = Castle(Square(e, _8), Square(g, _8), SimpleMove(Square(h, _8), Square(f, _8)))
  private val blackQueenside = Castle(Square(e, _8), Square(c, _8), SimpleMove(Square(a, _8), Square(d, _8)))

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
  override def toString: String = if (end.file == File.g) "O-O" else "O-O-O"
}
