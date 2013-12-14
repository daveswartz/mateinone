package mateinone

object Square {
  def offset(s: Square, f: File => Option[File], r: Rank => Option[Rank]): Option[Square] =
    f(s.file).flatMap(fo => r(s.rank).map(ro => Square(fo, ro)))
}

case class Square(file: File, rank: Rank)

// TODO now a move is associated with a single piece, change to be Move(moves: Set[Piece => Piece]) for castling and promotion
case class Move(start: Square, end: Square)

object PieceType extends Enumeration {
  type PieceType = Value
  val Pawn, Rook, Knight, Bishop, King, Queen = Value
}
import PieceType._

case class OccupiedPath(path: Path, occupied: Set[Square]) {
  def validEnds: Set[Square] = {
    def firstOccupiedInx = occupied.map(s => path.indexOf(s)).min
    if (occupied.isEmpty) path.toSet else path.splitAt(firstOccupiedInx)._1.toSet
  }
  def isValidEnd(end: Square): Boolean = validEnds.contains(end)
}

case class Piece(pieceType: PieceType, square: Square, occupiedPaths: Set[OccupiedPath], pathsFrom: Square => Set[Path]) {

  def isValidMove(move: Move): Boolean =
    if (move.start == square) {
      occupiedPaths.find(_.path.contains(move.end)).fold(false)(_.isValidEnd(move.end))
    } else false

  def otherPieceMoved(move: Move): Piece = {
    def vacated(o: OccupiedPath) = if (o.path.contains(move.start)) o.copy(occupied = o.occupied - move.start) else o
    def occupied(o: OccupiedPath) = if (o.path.contains(move.end)) o.copy(occupied = o.occupied + move.end) else o
    copy(occupiedPaths = occupiedPaths.map(vacated).map(occupied))
  }

  def moves: Set[Move] = occupiedPaths.flatMap(_.validEnds.map(Move(square, _)))

}

object Board {

  def apply(): Board = {
    val pawnSquares = File.allFiles.map(Square(_, `2`)).toSet
    val rookSquares = Set(A, H).map(Square(_, `1`))
    val knightSquares = Set(B, G).map(Square(_, `1`))
    val bishopSquares = Set(C, F).map(Square(_, `1`))
    val queenSquare = Set(Square(D, `1`))
    val kingSquare = Set(Square(E, `1`))

    val allOccupied = pawnSquares ++ rookSquares ++ knightSquares ++ bishopSquares ++ queenSquare ++ kingSquare

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

    def piecesFor(pieceType: PieceType, squares: Set[Square], pathsFrom: Square => Set[Path]) = {
      squares.map { square =>
        val occupiedPaths = pathsFrom(square).map(p => OccupiedPath(p, (allOccupied - square).filter(p.contains)))
        Piece(pieceType, square, occupiedPaths, pathsFrom)
      }
    }

    val pawns = piecesFor(
      Pawn,
      pawnSquares,
      (s) => {
        val single = path(s, Square.offset(_, File.identity, Rank.inc), 1)
        if (s.rank == `2`) Set(single, path(s, Square.offset(_, File.identity, Rank.inc), 2)) else Set(single)
      }
    )
    val rooks = piecesFor(
      Rook,
      rookSquares,
      (s) => file(s) ++ rank(s)
    )
    val knights = {
      def pathsFrom(s: Square) = Set((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
        .flatMap { case (f, r) => Square.offset(s, File.offset(_, f), Rank.offset(_, r)) }.map(List(_))
      piecesFor(Knight, knightSquares, pathsFrom)
    }
    val bishops = piecesFor(
      Bishop,
      bishopSquares,
      (s) => diagonals(s)
    )
    val king = piecesFor(
      King,
      kingSquare,
      (s) => Set(
        path(s, Square.offset(_, File.identity, Rank.inc), 1),
        path(s, Square.offset(_, File.inc, Rank.inc), 1),
        path(s, Square.offset(_, File.inc, Rank.identity), 1),
        path(s, Square.offset(_, File.inc, Rank.dec), 1),
        path(s, Square.offset(_, File.identity, Rank.dec), 1),
        path(s, Square.offset(_, File.dec, Rank.dec), 1),
        path(s, Square.offset(_, File.dec, Rank.identity), 1),
        path(s, Square.offset(_, File.dec, Rank.inc), 1)
      )
    )
    val queen = piecesFor(
      Queen,
      queenSquare,
      (s) => file(s) ++ rank(s) ++ diagonals(s)
    )

    new Board(pawns ++ rooks ++ knights ++ bishops ++ king ++ queen)
  }

}

case class Board(pieces: Set[Piece]) {

  def move(move: Move): Option[Board] = {
    val piece = pieces.find(_.square == move.start)
    val isValidMove = piece.fold(false)(_.isValidMove(move))
    if (isValidMove) {
      val others = pieces -- piece
      val allOccupied = others.map(_.square)
      val updatedOthers = others.map(_.otherPieceMoved(move))
      val updatedMover = piece.map { piece =>
        val occupiedPaths = piece.pathsFrom(move.end).map(path => OccupiedPath(path, allOccupied.filter(path.contains)))
        piece.copy(square = move.end, occupiedPaths = occupiedPaths)
      }
      Some(copy(pieces = updatedOthers ++ updatedMover))
    } else {
      None
    }
  }

  def moves: Set[Move] = pieces.flatMap(_.moves)

}
