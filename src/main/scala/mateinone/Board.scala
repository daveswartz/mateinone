package mateinone

object Square {
  def offset(s: Square, f: File => Option[File], r: Rank => Option[Rank]): Option[Square] =
    f(s.file).flatMap(fo => r(s.rank).map(ro => Square(fo, ro)))
}

case class Square(file: File, rank: Rank)

sealed trait PieceType

object PromotionType { val all = Set(Knight, Rook, Bishop, Queen) }
sealed trait PromotionType extends PieceType

case object Pawn extends PieceType
case object King extends PieceType
case object Knight extends PromotionType
case object Rook extends PromotionType
case object Bishop extends PromotionType
case object Queen extends PromotionType

case class Piece(pieceType: PieceType, square: Square)

sealed trait Move {
  val piece: Piece
  val end: Square
  def start: Square = piece.square
}
case class SimpleMove(piece: Piece, end: Square) extends Move
case class Promotion(piece: Piece, end: Square, promotionType: PromotionType) extends Move
object Castle {
  def unapply(castle: Castle): Option[(Piece, Square, SimpleMove)] = Some((castle.piece, castle.end, castle.rookMove))
}
sealed trait Castle extends Move { val rookMove: SimpleMove }
case object KingsideCastle extends Castle {
  val piece = Piece(King, Square(E, `1`))
  val end = Square(G, `1`)
  val rookMove = SimpleMove(Piece(Rook, Square(H, `1`)), Square(F, `1`))
}
case object QueensideCastle extends Castle {
  val piece = Piece(King, Square(E, `1`))
  val end = Square(C, `1`)
  val rookMove = SimpleMove(Piece(Rook, Square(A, `1`)), Square(D, `1`))
}

object Board {

  private type Path = List[Square]

  case class OccupiedPath(private val path: Path, private val occupied: Set[Square]) {
    def contains(s: Square): Boolean = path.contains(s)
    def vacated(s: Square): OccupiedPath = if (contains(s)) copy(occupied = occupied - s) else this
    def occupied(s: Square) = if (contains(s)) copy(occupied = occupied + s) else this
    def validEnds: Set[Square] = {
      def firstOccupiedInx = occupied.map(s => path.indexOf(s)).min
      if (occupied.isEmpty) path.toSet else path.splitAt(firstOccupiedInx)._1.toSet
    }
    def isValidEnd(end: Square): Boolean = validEnds.contains(end)
  }

  private def occupiedPathsFor(piece: Piece, others: Set[Piece], moved: Set[Piece]): Set[OccupiedPath] = {

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
        if (!moved.contains(piece))
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
        if (!moved.contains(piece)) {
          def kingsideRookMoved = others.find(_.square == Square(G, `1`)).fold(false)(!moved.contains(_))
          val kingsideCastle = path(piece.square, Square.offset(_, File.offset(_, 2), Rank.identity), 1)
          if (kingsideRookMoved) kingPaths = kingPaths + kingsideCastle
          def queensideRookMoved = others.find(_.square == Square(A, `1`)).fold(false)(!moved.contains(_))
          val queensideCastle = path(piece.square, Square.offset(_, File.offset(_, -2), Rank.identity), 1)
          if (queensideRookMoved) kingPaths = kingPaths + queensideCastle
        }
        kingPaths
      case Queen =>
        file(piece.square) ++ rank(piece.square) ++ diagonals(piece.square)
    }

    pathsForPiece.map(p => OccupiedPath(p, others.map(_.square).filter(p.contains)))

  }

  def apply(): Board = {
    def piece(pieceType: PieceType)(square: Square): Piece = Piece(pieceType, square)
    val noneMoved = Set.empty[Piece]
    def occupiedPaths(piece: Piece, others: Set[Piece]): Set[OccupiedPath] = occupiedPathsFor(piece, others, noneMoved)

    val pawns = File.allFiles.map(Square(_, `2`)).toSet.map(piece(Pawn))
    val rooks = Set(A, H).map(Square(_, `1`)).map(piece(Rook))
    val knights = Set(B, G).map(Square(_, `1`)).map(piece(Knight))
    val bishops = Set(C, F).map(Square(_, `1`)).map(piece(Bishop))
    val queen = Set(Square(D, `1`)).map(piece(Queen))
    val king = Set(Square(E, `1`)).map(piece(King))

    val pieces = pawns ++ rooks ++ knights ++ bishops ++ king ++ queen
    val initialPiecesToOccupiedPaths = pieces.map(piece => (piece, occupiedPaths(piece, pieces - piece))).toMap

    new Board {
      val piecesToOccupiedPaths = initialPiecesToOccupiedPaths
      val moved = noneMoved
    }
  }

}

trait Board {
  import Board._

  protected val piecesToOccupiedPaths: Map[Piece, Set[OccupiedPath]]

  protected val moved: Set[Piece]

  def pieces: Set[Piece] = piecesToOccupiedPaths.keys.toSet

  private def canPromote(piece: Piece, end: Square): Boolean = piece.pieceType == Pawn && end.rank == `8`

  def move(move: Move): Option[Board] = {

    piecesToOccupiedPaths.get(move.piece).flatMap { occupiedPaths =>

      def isValidEnd(move: Move): Boolean = occupiedPaths.find(_.contains(move.end)).fold(false)(_.isValidEnd(move.end))

      def isValidMove: Boolean = {
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
               .map { o => o.vacated(move.start); o.occupied(move.end) }
               .map { o =>
               move match {
                 case Castle(_, _, rookMove) => o.vacated(rookMove.start); o.occupied(rookMove.end)
                 case _ => o
               }
             })
        }

        val moversUpdated = {
          def withOccupiedPaths(p: Piece): (Piece, Set[OccupiedPath]) = (p, occupiedPathsFor(p, pieces - p, moved))
          move match {
            case SimpleMove(_, _) =>
              Set(withOccupiedPaths(move.piece.copy(square = move.end)))
            case Promotion(_, _, promotionType) =>
              Set(withOccupiedPaths(move.piece.copy(square = move.end, pieceType = promotionType)))
            case Castle(_, _, rookMove) =>
              Set(
                withOccupiedPaths(move.piece.copy(square = move.end)),
                withOccupiedPaths(rookMove.piece.copy(square = rookMove.end))
              )
          }
        }

        val movedUpdated = moved ++ (move match { case Castle(_, _, rookMove) => Set(move.piece, rookMove.piece) case _ => Set(move.piece) })

        Some(
          new Board {
            val piecesToOccupiedPaths = othersUpdated ++ moversUpdated
            val moved = movedUpdated
          }
        )
      } else {
        None
      }
    }
  }

  // TODO Generate castling moves
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
