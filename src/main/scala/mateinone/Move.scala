package mateinone

import Square._

sealed trait Move { val start: Square; val end: Square }

object SimpleMove {
  implicit def tupleToSimpleMove(t: (Square, Square)): SimpleMove = t match { case (start, end) => SimpleMove(start, end) }
}
case class SimpleMove(start: Square, end: Square) extends Move {
  def promote(promotionType: PromotionType): Promotion = Promotion(start, end, promotionType)
}

case class Promotion(start: Square, end: Square, promotionType: PromotionType) extends Move

object Castle {
  val whiteKingside = Castle(e1, g1, SimpleMove(h1, f1))
  val whiteQueenside = Castle(e1, c1, SimpleMove(a1, d1))
  val blackKingside = Castle(e8, g8, SimpleMove(h8, f8))
  val blackQueenside = Castle(e8, c8, SimpleMove(a8, d8))
  def `O-O`(side: Side) = side match {
    case White => whiteKingside
    case Black => blackKingside
  }
  def `O-O-O`(side: Side) = side match {
    case White => whiteQueenside
    case Black => blackQueenside
  }
}
case class Castle private(start: Square, end: Square, rookMove: SimpleMove) extends Move