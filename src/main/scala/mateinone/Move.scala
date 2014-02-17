package mateinone

sealed trait Move
case class SimpleMove(start: Square, end: Square) extends Move
case class Promotion(start: Square, end: Square, promotionType: PromotionType) extends Move
sealed trait Castle extends Move
case object `O-O` extends Castle
case object `O-O-O` extends Castle

object MoveImplicits {
  implicit def tupleToSimpleMove(t: (Square, Square)) = t match { case (start, end) => SimpleMove(start, end) }
  implicit def tupleTupleToPromotion(t: ((Square, Square), PromotionType)) = t match { case ((start, end), promotionType) => Promotion(start, end, promotionType) }
}
