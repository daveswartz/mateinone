package mateinone

import Square._

object Move {
  implicit def moveToEither(m: Move): Either[Move, Side => Move] = Left(m)
  def toMove(e: Either[Move, Side => Move], s: Side): Move = e match { case Left(l) => l case Right(r) => r(s)}
}
sealed trait Move { val start: Square; val end: Square }

object SimpleMove {
  implicit def tupleToSimpleMove(t: (Square, Square)): SimpleMove = t match { case (start, end) => SimpleMove(start, end) }
  implicit def tupleToLeftSimpleMove(t: (Square, Square)): Either[Move, Side => Move] = t match { case (start, end) => Left(SimpleMove(start, end)) }
}
case class SimpleMove(start: Square, end: Square) extends Move {
  def promote(promotionType: PromotionType): Either[Move, Side => Move] =
    Left(Promotion.optionally(start, end, promotionType).get) // TODO unsafe get here can throw exception
}

object Promotion {

  val all = {
    Set(a7, b7, c7, d7, e7, f7, g7, h7, a2, b2, c2, d2, e2, f2, g2, h2)
      .zip(Set(a8, b8, c8, d8, e8, f8, g8, h8, a1, b1, c1, d1, e1, f1, g1, h1))
      .zip(PromotionType.all)
      .map { case ((start, end), promotionType) => Promotion(start, end, promotionType) }
  }

  def optionally(start: Square, end: Square, promotionType: PromotionType): Option[Promotion] =
    Some(Promotion(start, end, promotionType)).filter(all.contains)

}
case class Promotion private(start: Square, end: Square, promotionType: PromotionType) extends Move

object Castle {

  val whiteKingside = Castle(e1, g1, SimpleMove(h1, f1))
  val whiteQueenside = Castle(e1, c1, SimpleMove(a1, d1))
  val blackKingside = Castle(e8, g8, SimpleMove(h8, f8))
  val blackQueenside = Castle(e8, c8, SimpleMove(a8, d8))

  val all = Set(whiteKingside, whiteQueenside, blackKingside, blackQueenside)

  val `O-O` = (side: Side) => side match {
    case White => whiteKingside
    case Black => blackKingside
  }

  val `O-O-O` = (side: Side) => side match {
    case White => whiteQueenside
    case Black => blackQueenside
  }

  def optionally(start: Square, end: Square, rookMove: SimpleMove): Option[Castle] =
    Some(Castle(start, end, rookMove)).filter(all.contains)

  implicit def castleToRightCastle(c: Side => Castle): Either[Move, Side => Move] = Right(c)

}
case class Castle private(start: Square, end: Square, rookMove: SimpleMove) extends Move