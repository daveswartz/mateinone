package mateinone

import scala.language.implicitConversions

sealed trait MoveBase
sealed trait StartAndEnd { val start: Square; val end: Square }
case class Move(start: Square, end: Square) extends MoveBase with StartAndEnd
case class Promotion(start: Square, end: Square, `type`: PromotionType) extends MoveBase with StartAndEnd
sealed trait Castle extends MoveBase
case object `O-O` extends Castle
case object `O-O-O` extends Castle

object MoveImplicits {
  implicit def tupleToSimpleMove(t: (Square, Square)) = t match { case (start, end) => Move(start, end) }
  implicit def tupleTupleToPromotion(t: ((Square, Square), PromotionType)) = t match { case ((start, end), promotion) => Promotion(start, end, promotion) }
}
