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
  def promote(promotionType: PromotionType): Either[Move, Side => Move] = Left(Promotion(start, end, promotionType))
}

case class Promotion(start: Square, end: Square, promotionType: PromotionType) extends Move

object Castle {
  val whiteKingside = Castle(e1, g1, SimpleMove(h1, f1))
  val whiteQueenside = Castle(e1, c1, SimpleMove(a1, d1))
  val blackKingside = Castle(e8, g8, SimpleMove(h8, f8))
  val blackQueenside = Castle(e8, c8, SimpleMove(a8, d8))
  val `O-O` = (side: Side) => side match {
    case White => whiteKingside
    case Black => blackKingside
  }
  val `O-O-O` = (side: Side) => side match {
    case White => whiteQueenside
    case Black => blackQueenside
  }
  implicit def castleToRightCastle(c: Side => Castle): Either[Move, Side => Move] = Right(c)
}
case class Castle private(start: Square, end: Square, rookMove: SimpleMove) extends Move