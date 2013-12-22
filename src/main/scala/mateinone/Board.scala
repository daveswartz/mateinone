package mateinone

import Square._

sealed trait PieceType

object PromotionType { val all = Set(Knight, Rook, Bishop, Queen) }
sealed trait PromotionType extends PieceType

case object Pawn extends PieceType
case object King extends PieceType
case object Knight extends PromotionType
case object Rook extends PromotionType
case object Bishop extends PromotionType
case object Queen extends PromotionType

case class Piece(pieceType: PieceType, square: Square, hasMoved: Boolean = false)

sealed trait Move { // TODO make move definition more succinct (e.g., c1 -> a3). May split how move represented and specified.
  val piece: Piece
  val end: Square
  def start: Square = piece.square
  def pieceAtEnd: Piece = piece.copy(square = end, hasMoved = true)
}
case class SimpleMove(piece: Piece, end: Square) extends Move
case class Promotion(piece: Piece, end: Square, promotionType: PromotionType) extends Move {
  override def pieceAtEnd: Piece = piece.copy(pieceType = promotionType, square = end, hasMoved = true)
}
object Castle {
  def unapply(castle: Castle): Option[(Piece, Square, SimpleMove)] = Some((castle.piece, castle.end, castle.rookMove))
}
sealed trait Castle extends Move { val rookMove: SimpleMove }
case object KingsideCastle extends Castle {
  val piece = Piece(King, e1)
  val end = g1
  val rookMove = SimpleMove(Piece(Rook, h1), f1)
}
case object QueensideCastle extends Castle {
  val piece = Piece(King, e1)
  val end = c1
  val rookMove = SimpleMove(Piece(Rook, a1), d1)
}

object Board {

  private type Path = List[Square]

  case class OccupiedPath(private val path: Path, private val occupied: Set[Square]) {
    def contains(s: Square): Boolean = path.contains(s)
    def vacate(s: Square): OccupiedPath = if (contains(s)) copy(occupied = this.occupied - s) else this
    def occupy(s: Square): OccupiedPath = if (contains(s)) copy(occupied = this.occupied + s) else this
    def validEnds: Set[Square] = {
      def firstOccupiedInx = occupied.map(s => path.indexOf(s)).min
      if (occupied.isEmpty) path.toSet else path.splitAt(firstOccupiedInx)._1.toSet
    }
    def isValidEnd(end: Square): Boolean = validEnds.contains(end)
  }

  private def occupiedPathsFor(piece: Piece, others: Set[Piece]): Set[OccupiedPath] = {

    def path(square: Square, step: Square => Option[Square], remaining: Int): Path = {
      def pathRecur(current: Path, step: Square => Option[Square], remaining: Int): Path = step(current.last) match {
        case Some(next) if remaining > 0 => pathRecur(current :+ next, step, remaining - 1)
        case _ => current
      }
      pathRecur(List(square), step, remaining).tail
    }

    def file(square: Square) = Set(
      path(square, Square.offset(_, File.inc, Rank.identity), 7),
      path(square, Square.offset(_, File.dec, Rank.identity), 7)
    )

    def rank(square: Square) = Set(
      path(square, Square.offset(_, File.identity, Rank.inc), 7),
      path(square, Square.offset(_, File.identity, Rank.dec), 7)
    )

    def diagonals(square: Square) = Set(
      path(square, Square.offset(_, File.inc, Rank.inc), 7),
      path(square, Square.offset(_, File.inc, Rank.dec), 7),
      path(square, Square.offset(_, File.dec, Rank.inc), 7),
      path(square, Square.offset(_, File.dec, Rank.dec), 7)
    )

    val pathsForPiece = piece.pieceType match {
      case Pawn =>
        val single = path(piece.square, Square.offset(_, File.identity, Rank.inc), 1)
        if (!piece.hasMoved)
          Set(single, path(piece.square, Square.offset(_, File.identity, Rank.inc), 2))
        else
          Set(single)
      case Rook =>
        file(piece.square) ++ rank(piece.square)
      case Knight =>
        Set((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
          .flatMap { case (f, r) => Square.offset(piece.square, File.offset(_, f), Rank.offset(_, r)) }.map(List(_))
      case Bishop =>
        diagonals(piece.square)
      case King =>
        var kingPaths = Set(
          path(piece.square, Square.offset(_, File.identity, Rank.inc), 1),
          path(piece.square, Square.offset(_, File.inc, Rank.inc), 1),
          path(piece.square, Square.offset(_, File.inc, Rank.identity), 1),
          path(piece.square, Square.offset(_, File.inc, Rank.dec), 1),
          path(piece.square, Square.offset(_, File.identity, Rank.dec), 1),
          path(piece.square, Square.offset(_, File.dec, Rank.dec), 1),
          path(piece.square, Square.offset(_, File.dec, Rank.identity), 1),
          path(piece.square, Square.offset(_, File.dec, Rank.inc), 1)
        )
        if (!piece.hasMoved) {
          def kingsideRookMoved = others.find(_.square == g1).fold(false)(!_.hasMoved)
          val kingsideCastle = path(piece.square, Square.offset(_, File.offset(_, 2), Rank.identity), 1)
          if (kingsideRookMoved) kingPaths = kingPaths + kingsideCastle
          def queensideRookMoved = others.find(_.square == a1).fold(false)(!_.hasMoved)
          val queensideCastle = path(piece.square, Square.offset(_, File.offset(_, -2), Rank.identity), 1)
          if (queensideRookMoved) kingPaths = kingPaths + queensideCastle
        }
        kingPaths
      case Queen =>
        file(piece.square) ++ rank(piece.square) ++ diagonals(piece.square)
    }

    pathsForPiece.filterNot(_.isEmpty).map(p => OccupiedPath(p, others.map(_.square).filter(p.contains)))

  }

  def apply(): Board = {
    def pieceOfType(pieceType: PieceType)(square: Square): Piece = Piece(pieceType, square)

    val pawns = File.allFiles.map(Square(_, `2`)).toSet.map(pieceOfType(Pawn))
    val rooks = Set(A, H).map(Square(_, `1`)).map(pieceOfType(Rook))
    val knights = Set(B, G).map(Square(_, `1`)).map(pieceOfType(Knight))
    val bishops = Set(C, F).map(Square(_, `1`)).map(pieceOfType(Bishop))
    val queen = Set(d1).map(pieceOfType(Queen))
    val king = Set(e1).map(pieceOfType(King))

    val pieces = pawns ++ rooks ++ knights ++ bishops ++ king ++ queen
    val initialPiecesToOccupiedPaths = pieces.map(piece => (piece, occupiedPathsFor(piece, pieces - piece))).toMap

    new Board { val piecesToOccupiedPaths = initialPiecesToOccupiedPaths }
  }

}

trait Board {
  import Board._

  protected val piecesToOccupiedPaths: Map[Piece, Set[OccupiedPath]]

  def pieces: Set[Piece] = piecesToOccupiedPaths.keys.toSet

  def piece(square: Square): Option[Piece] = pieces.find(square == _.square)

  private def canPromote(piece: Piece, end: Square): Boolean = piece.pieceType == Pawn && end.rank == `8`

  def move(move: Move): Option[Board] =
    piecesToOccupiedPaths.get(move.piece).flatMap { occupiedPaths =>

      def isValidMove: Boolean = {
        def isValidEnd(move: Move): Boolean = occupiedPaths.find(_.contains(move.end)).fold(false)(_.isValidEnd(move.end))
        move match {
          case _: SimpleMove => isValidEnd(move)
          case _: Promotion => isValidEnd(move) && canPromote(move.piece, move.end)
          case Castle(_, _, rookMove) => isValidEnd(move) && isValidEnd(rookMove)
        }
      }

      if (isValidMove) {

        val others = piecesToOccupiedPaths -- (move match {
          case Castle(_, _, rookMove) => Set(move.piece, rookMove.piece)
          case _ => Set(move.piece)
        })

        val othersUpdated = others.map {
          case (otherPiece, otherOccupiedPaths) =>
            (otherPiece,
             otherOccupiedPaths
               .map(_.vacate(move.start).occupy(move.end))
               .map { o =>
               move match {
                 case Castle(_, _, rookMove) => o.vacate(rookMove.start).occupy(rookMove.end)
                 case _ => o
               }
             })
        }

        val moversUpdated = {
          def withOccupiedPaths(p: Piece): (Piece, Set[OccupiedPath]) = (p, occupiedPathsFor(p, others.keys.toSet))
          move match {
            case Castle(_, _, rookMove) =>
              Set(withOccupiedPaths(move.pieceAtEnd), withOccupiedPaths(rookMove.pieceAtEnd))
            case _ =>
              Set(withOccupiedPaths(move.pieceAtEnd))
          }
        }

        Some( new Board { val piecesToOccupiedPaths = othersUpdated ++ moversUpdated } )
      } else None

    }

  def moves(moves: List[Move]): Option[Board] = moves match {
    case Nil => Some(this)
    case move :: Nil => this.move(move)
    case head :: tail => this.move(head).flatMap(_.moves(tail))
  }

  def moves: Set[Move] =
    piecesToOccupiedPaths.map {
      case (piece, occupiedPaths) =>
        occupiedPaths.map { occupiedPath =>
          occupiedPath.validEnds.map { end: Square =>
            if (canPromote(piece, end))
              PromotionType.all.map(promotionType => Promotion(piece, end, promotionType))
            else
              Set(SimpleMove(piece, end))
          }
        }
    }.toSet.flatten.flatten.flatten

}
