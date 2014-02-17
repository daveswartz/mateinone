package mateinone

import Square._
import File._
import Rank._

object Move {

  implicit def moveToEither(m: Move): Either[Move, Side => Move] = Left(m)

  def toMove(e: Either[Move, Side => Move], s: Side): Move = e match { case Left(l) => l case Right(r) => r(s)}

  def moves(start: Square, end: Square, pieceType: PieceType): Set[_ <: Move] = {
    def default(args: (Square, Square, PieceType)) = Set(SimpleMove(args._1, args._2))
    Promotion.promotion.orElse(Castle.castle).applyOrElse((start, end, pieceType), default)
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
  def promote(promotionType: PromotionType): Option[Promotion] = Promotion.promotion.lift((start, end, Pawn)).flatMap(_.find(p => p.promotionType == promotionType))
}

object Promotion {

  private def promotions(starts: List[Square], rankOffset: Int) = for {
    start <- starts
    fileOffset <- List(-1, 0, 1)
    promotionType <- PromotionType.all
    end = start + (fileOffset, rankOffset)
    if end.isDefined
  } yield Promotion(start, end.get, promotionType)

  private val all = promotions(files.toList.map(square(_,_7)), 1) ++ promotions(files.toList.map(square(_,_2)), -1)

  val promotion = new PartialFunction[(Square, Square, PieceType), Set[Promotion]] {
    override def isDefinedAt(args: (Square, Square, PieceType)): Boolean = args._3 == Pawn && all.exists(p => args._1 == p.start && args._2 == p.end)
    override def apply(args: (Square, Square, PieceType)): Set[Promotion] = PromotionType.all.map(t => Promotion(args._1, args._2, t))
  }

  implicit def promotionToLeftPromotion(p: Promotion): Either[Promotion, Side => Promotion] = Left(p)

}
case class Promotion private(start: Square, end: Square, promotionType: PromotionType) extends Move

object Castle {

  private val whiteKingside = Castle(E1, G1, SimpleMove(H1, F1))
  private val whiteQueenside = Castle(E1, C1, SimpleMove(A1, D1))
  private val blackKingside = Castle(E8, G8, SimpleMove(H8, F8))
  private val blackQueenside = Castle(E8, C8, SimpleMove(A8, D8))

  val all = Set(whiteKingside, whiteQueenside, blackKingside, blackQueenside)

  val castle = new PartialFunction[(Square, Square, PieceType), Set[Castle]] {
    override def isDefinedAt(args: (Square, Square, PieceType)): Boolean = args._3 == King && all.exists(c => args._1 == c.start && args._2 == c.end)
    override def apply(args: (Square, Square, PieceType)): Set[Castle] = all.filter(c => args._1 == c.start && args._2 == c.end)
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
case class Castle private(start: Square, end: Square, rookMove: SimpleMove) extends Move
