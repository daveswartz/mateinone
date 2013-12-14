package mateinone

object Square {
  def offset(s: Square, f: File => Option[File], r: Rank => Option[Rank]): Option[Square] =
    f(s.file).flatMap(fo => r(s.rank).map(ro => Square(fo, ro)))
}

case class Square(file: File, rank: Rank)

sealed trait PieceType
case object Pawn extends PieceType
case object King extends PieceType
object PromotionType { val all = Set(Knight, Rook, Bishop, Queen) }
sealed trait PromotionType extends PieceType
case object Knight extends PromotionType
case object Rook extends PromotionType
case object Bishop extends PromotionType
case object Queen extends PromotionType

sealed trait Move { val start: Square; val end: Square }
case class SimpleMove(start: Square, end: Square) extends Move
case class Promotion(start: Square, end: Square, promotionType: PromotionType) extends Move

case class OccupiedPath(path: Path, occupied: Set[Square]) {
  def validEnds: Set[Square] = {
    def firstOccupiedInx = occupied.map(s => path.indexOf(s)).min
    if (occupied.isEmpty) path.toSet else path.splitAt(firstOccupiedInx)._1.toSet
  }
  def isValidEnd(end: Square): Boolean = validEnds.contains(end)
}

case class Piece(pieceType: PieceType, square: Square, occupiedPaths: Set[OccupiedPath]) {

  private def canPromote(end: Rank): Boolean = pieceType == Pawn && end == `8`

  def isValidMove(move: Move): Boolean = {
    def areStartAndEndValid(move: Move): Boolean = if (move.start == square) {
      occupiedPaths.find(_.path.contains(move.end)).fold(false)(_.isValidEnd(move.end))
    } else false
    move match {
      case m: SimpleMove => areStartAndEndValid(m)
      case m: Promotion => areStartAndEndValid(m) && canPromote(m.end.rank)
    }
  }

  def otherPieceMoved(move: Move): Piece = {
    def vacated(o: OccupiedPath) = if (o.path.contains(move.start)) o.copy(occupied = o.occupied - move.start) else o
    def occupied(o: OccupiedPath) = if (o.path.contains(move.end)) o.copy(occupied = o.occupied + move.end) else o
    copy(occupiedPaths = occupiedPaths.map(vacated).map(occupied))
  }

  def moves: Set[Move] = occupiedPaths.flatMap(path =>
    path.validEnds.map { end =>
      if (canPromote(end.rank))
        PromotionType.all.map(t => Promotion(square, end, t))
      else
        Set(SimpleMove(square, end))
    }
  ).flatten

}

object Board {

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

      (s: Square) => pieceType match {
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
        Piece(pieceType, square, occupiedPaths)
      }
    }

    val pawns = piecesFor(Pawn, pawnSquares)
    val rooks = piecesFor(Rook, rookSquares)
    val knights = piecesFor(Knight, knightSquares)
    val bishops = piecesFor(Bishop, bishopSquares)
    val king = piecesFor(King, kingSquare)
    val queen = piecesFor(Queen, queenSquare)

    new Board(pawns ++ rooks ++ knights ++ bishops ++ king ++ queen)
  }

}

case class Board(pieces: Set[Piece]) {
  import Board._

  def move(move: Move): Option[Board] = {
    val piece = pieces.find(_.square == move.start)
    val isValidMove = piece.fold(false)(_.isValidMove(move))
    if (isValidMove) {
      val others = pieces -- piece
      val allOthersOccupied = others.map(_.square)
      val updatedOthers = others.map(_.otherPieceMoved(move))
      val updatedMover = piece.map { piece =>
        move match {
          case m: SimpleMove =>
            val occupiedPaths = occupiedPathsFrom(piece.pieceType, allOthersOccupied)(m.end)
            piece.copy(square = move.end, occupiedPaths = occupiedPaths)
          case m: Promotion =>
            val occupiedPaths = occupiedPathsFrom(m.promotionType, allOthersOccupied)(m.end)
            piece.copy(pieceType = m.promotionType, square = move.end, occupiedPaths = occupiedPaths)
        }
      }
      Some(copy(pieces = updatedOthers ++ updatedMover))
    } else {
      None
    }
  }

  def moves: Set[Move] = pieces.flatMap(_.moves)

}
