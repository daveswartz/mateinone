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
  val `O-O` = Castle(e1, g1, SimpleMove(h1, f1)) // TODO white side is hard-coded
  val `O-O-O` = Castle(e1, c1, SimpleMove(a1, d1)) // TODO white side is hard-coded
}
case class Castle private(start: Square, end: Square, rookMove: SimpleMove) extends Move