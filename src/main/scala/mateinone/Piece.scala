package mateinone

sealed trait PieceType
case object Pawn extends PieceType
case object King extends PieceType

object PromotionType { val all = Vector(Knight, Rook, Bishop, Queen) }
sealed trait PromotionType extends PieceType
case object Knight extends PromotionType
case object Rook extends PromotionType
case object Bishop extends PromotionType
case object Queen extends PromotionType

sealed trait Color
case object White extends Color
case object Black extends Color
