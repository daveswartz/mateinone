package mateinone

sealed trait PieceType

object PromotionType { val all = Set(Knight, Rook, Bishop, Queen) }
sealed trait PromotionType extends PieceType

case object Pawn extends PieceType { override def toString: String = "♙" }
case object King extends PieceType { override def toString: String = "♔" }
case object Knight extends PromotionType { override def toString: String = "♘" }
case object Rook extends PromotionType { override def toString: String = "♖" }
case object Bishop extends PromotionType { override def toString: String = "♗" }
case object Queen extends PromotionType { override def toString: String = "♕" }

sealed trait Side { val other: Side }
case object White extends Side { val other: Side = Black }
case object Black extends Side { val other: Side = White }

case class Piece(side: Side, pieceType: PieceType, square: Square) {
  def promotedTo(promotionType: PromotionType): Piece = copy(pieceType = promotionType)
}
