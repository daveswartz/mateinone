package mateinone

import scala.language.implicitConversions
import scala.language.reflectiveCalls

object MoveBase {
  private val fileStrings = Vector("a", "b", "c", "d", "e", "f", "g", "h")
  implicit def squareToString(s: Square) = new { def print: String = fileStrings(s.file.n) + (s.rank.n + 1).toString }
}
sealed trait MoveBase
sealed trait StartAndEnd { val start: Square; val end: Square }

case class Move(start: Square, end: Square) extends MoveBase with StartAndEnd {
  import MoveBase.squareToString
  override def toString(): String = start.print+"->"+end.print
}

object Promotion {
  implicit def promotionTypeToString(p: PromotionType) =
    new { def print: String = p match { case Rook => "♖"; case Knight => "♘"; case Bishop => "♗"; case Queen => "♕" } }
}
case class Promotion(start: Square, end: Square, `type`: PromotionType) extends MoveBase with StartAndEnd {
  import MoveBase.squareToString
  import Promotion.promotionTypeToString
  override def toString(): String = start.print+"->"+end.print+"="+`type`.print
}

sealed trait Castle extends MoveBase
case object `O-O` extends Castle { override def toString(): String = "O-O" }
case object `O-O-O` extends Castle { override def toString(): String = "O-O-O" }

object MoveImplicits {
  implicit def tupleToSimpleMove(t: (Square, Square)) = t match { case (start, end) => Move(start, end) }
  implicit def tupleTupleToPromotion(t: ((Square, Square), PromotionType)) = t match { case ((start, end), promotion) => Promotion(start, end, promotion) }
}
