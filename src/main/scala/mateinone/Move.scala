package mateinone

import scala.language.implicitConversions
import scala.language.reflectiveCalls

sealed trait MoveBase
sealed trait StartAndEnd { val start: Square; val end: Square }

case class Move(start: Square, end: Square) extends MoveBase with StartAndEnd {
  override def toString(): String = start+"->"+end
}

object Promotion {
  implicit def promotionTypeToString(p: PromotionType) =
    new { def print: String = p match { case Rook => "♖"; case Knight => "♘"; case Bishop => "♗"; case Queen => "♕" } }
}
case class Promotion(start: Square, end: Square, `type`: PromotionType) extends MoveBase with StartAndEnd {
  import Promotion.promotionTypeToString
  override def toString(): String = start+"->"+end+"="+`type`.print
}

sealed trait Castle extends MoveBase
case object `O-O` extends Castle { override def toString(): String = "O-O" }
case object `O-O-O` extends Castle { override def toString(): String = "O-O-O" }

object MoveImplicits {
  implicit def tupleToSimpleMove(t: (Square, Square)) = t match { case (start, end) => Move(start, end) }
  implicit def tupleTupleToPromotion(t: ((Square, Square), PromotionType)) = t match { case ((start, end), promotion) => Promotion(start, end, promotion) }
}
