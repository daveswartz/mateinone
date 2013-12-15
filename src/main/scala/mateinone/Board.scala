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

object Board {

  type Path = List[Square]

  case class OccupiedPath(path: Path, occupied: Set[Square]) {
    def validEnds: Set[Square] = {
      def firstOccupiedInx = occupied.map(s => path.indexOf(s)).min
      if (occupied.isEmpty) path.toSet else path.splitAt(firstOccupiedInx)._1.toSet
    }
    def isValidEnd(end: Square): Boolean = validEnds.contains(end)
  }

  // TODO put in board class and make use of board state (eventually will need for pins, castling)
  def occupiedPathsFrom(pieceType: PieceType, allOthersOccupied: Set[Square]): Square => Set[OccupiedPath] = {
    def pathsFrom(pieceType: PieceType): Square => Set[Path] = {
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

      (s: Square) => pieceType match { // TODO make this function of the board state not just a square
        case Pawn =>
          val single = path(s, Square.offset(_, File.identity, Rank.inc), 1)
          if (s.rank == `2`) Set(single, path(s, Square.offset(_, File.identity, Rank.inc), 2)) else Set(single)
        case Rook =>
          file(s) ++ rank(s)
        case Knight =>
          Set((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
            .flatMap { case (f, r) => Square.offset(s, File.offset(_, f), Rank.offset(_, r)) }.map(List(_))
        case Bishop =>
          diagonals(s)
        case King =>
          Set(
            path(s, Square.offset(_, File.identity, Rank.inc), 1),
            path(s, Square.offset(_, File.inc, Rank.inc), 1),
            path(s, Square.offset(_, File.inc, Rank.identity), 1),
            path(s, Square.offset(_, File.inc, Rank.dec), 1),
            path(s, Square.offset(_, File.identity, Rank.dec), 1),
            path(s, Square.offset(_, File.dec, Rank.dec), 1),
            path(s, Square.offset(_, File.dec, Rank.identity), 1),
            path(s, Square.offset(_, File.dec, Rank.inc), 1)
          )
        case Queen =>
          file(s) ++ rank(s) ++ diagonals(s)
      }
    }
    (s) => pathsFrom(pieceType)(s).map(p => OccupiedPath(p, allOthersOccupied.filter(p.contains)))
  }

  def apply(): Board = {
    val pawnSquares = File.allFiles.map(Square(_, `2`)).toSet
    val rookSquares = Set(A, H).map(Square(_, `1`))
    val knightSquares = Set(B, G).map(Square(_, `1`))
    val bishopSquares = Set(C, F).map(Square(_, `1`))
    val queenSquare = Set(Square(D, `1`))
    val kingSquare = Set(Square(E, `1`))

    val allOccupied = pawnSquares ++ rookSquares ++ knightSquares ++ bishopSquares ++ queenSquare ++ kingSquare

    def piecesFor(pieceType: PieceType, squares: Set[Square]) = {
      squares.map { square =>
        val allOthersOccupied = allOccupied - square
        val occupiedPaths = occupiedPathsFrom(pieceType, allOthersOccupied)(square)
        (Piece(pieceType, square), occupiedPaths)
      }
    }

    val pawns = piecesFor(Pawn, pawnSquares)
    val rooks = piecesFor(Rook, rookSquares)
    val knights = piecesFor(Knight, knightSquares)
    val bishops = piecesFor(Bishop, bishopSquares)
    val king = piecesFor(King, kingSquare)
    val queen = piecesFor(Queen, queenSquare)

    new Board { val piecesToOccupiedPaths = (pawns ++ rooks ++ knights ++ bishops ++ king ++ queen).toMap }
  }

}

trait Board {
  import Board._

  protected val piecesToOccupiedPaths: Map[Piece, Set[OccupiedPath]]

  def pieces: Set[Piece] = piecesToOccupiedPaths.keys.toSet

  private def canPromote(piece: Piece, end: Square): Boolean = piece.pieceType == Pawn && end.rank == `8`

  def move(move: Move): Option[Board] = {

    piecesToOccupiedPaths.get(move.piece).flatMap { occupiedPaths =>

      def isValidMove: Boolean = {
        def isValidEnd: Boolean = occupiedPaths.find(_.path.contains(move.end)).fold(false)(_.isValidEnd(move.end))
        move match {
          case _: SimpleMove => isValidEnd
          case _: Promotion => isValidEnd && canPromote(move.piece, move.end)
        }
      }

      if (isValidMove) {
        val others = piecesToOccupiedPaths - move.piece

        val othersUpdated = others.map { case (otherPiece, otherOccupiedPaths) =>
          def vacated(o: OccupiedPath) = if (o.path.contains(move.start)) o.copy(occupied = o.occupied - move.start) else o
          def occupied(o: OccupiedPath) = if (o.path.contains(move.end)) o.copy(occupied = o.occupied + move.end) else o
          (otherPiece, otherOccupiedPaths.map(vacated).map(occupied))
        }

        val otherSquares = others.keys.map(_.square).toSet

        val moverUpdated = move match {
          case SimpleMove(_, _) =>
            (move.piece.copy(square = move.end), occupiedPathsFrom(move.piece.pieceType, otherSquares)(move.end))
          case Promotion(_, _, promotionType) =>
            (move.piece.copy(square = move.end, pieceType = promotionType), occupiedPathsFrom(promotionType, otherSquares)(move.end))
        }

        Some(new Board { val piecesToOccupiedPaths = othersUpdated + moverUpdated })
      } else {
        None
      }
    }
  }

  def moves: Set[Move] =
    piecesToOccupiedPaths.map { case (piece, occupiedPaths) =>
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
